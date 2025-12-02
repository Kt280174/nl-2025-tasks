# Clean Pulse Monitor – no output glitch
# All workers send log messages to a dedicated logger process.

defmodule PulseMonitor do
  def run(config_path \\ "pulses.txt") do
    config_path =
      case System.argv() do
        [path | _] -> path
        _ -> config_path
      end

    configs =
      case File.read(config_path) do
        {:ok, contents} ->
          IO.puts("Using config file: #{config_path}")
          parse_config(contents)

        {:error, reason} ->
          IO.puts("WARNING: cannot read #{config_path} (#{inspect(reason)}).")
          IO.puts("Using built-in default config instead.\n")
          default_config()
      end

    logger = spawn(fn -> logger_loop() end)
    parent = self()

    workers =
      Enum.map(configs, fn cfg ->
        spawn(fn -> worker_loop(parent, logger, cfg.name, cfg.interval, cfg.count) end)
      end)

    total_pulses =
      Enum.reduce(configs, 0, fn cfg, acc -> acc + cfg.count end)

    send(logger, {:log, "Spawned #{length(workers)} workers, expect #{total_pulses} pulses.\n"})

    monitor_loop(%{
      logger: logger,
      total: total_pulses,
      received: 0,
      done: 0,
      expected_workers: length(workers)
    })
  end

  # ---------- Config parsing ----------

  defp parse_config(contents) do
    contents
    |> String.split("\n", trim: true)
    |> Enum.reject(&skip_line?/1)
    |> Enum.map(fn line ->
      [name, interval, count] = String.split(line)
      %{name: name, interval: String.to_integer(interval), count: String.to_integer(count)}
    end)
  end

  defp skip_line?(line),
    do: String.trim(line) == "" or String.starts_with?(String.trim(line), "#")

  defp default_config do
    [
      %{name: "sensor_a", interval: 500, count: 5},
      %{name: "sensor_b", interval: 800, count: 3},
      %{name: "sensor_c", interval: 1200, count: 4}
    ]
  end

  # ---------- Logger process ----------

  defp logger_loop do
    receive do
      {:log, text} ->
        IO.puts(text)
        logger_loop()
    end
  end

  # ---------- Worker process ----------

  defp worker_loop(parent, logger, name, interval, remaining) when remaining > 0 do
    :timer.sleep(interval)
    send(parent, {:pulse, name, remaining - 1})
    send(logger, {:log, "[#{timestamp()}] pulse from #{name}, remaining=#{remaining - 1}"})
    worker_loop(parent, logger, name, interval, remaining - 1)
  end

  defp worker_loop(parent, logger, name, _interval, 0) do
    send(parent, {:done, name})
    send(logger, {:log, "[#{timestamp()}] #{name} finished."})
  end

  # ---------- Monitor loop ----------

  defp monitor_loop(state) do
    receive do
      {:pulse, _name, _rem} ->
        monitor_loop(%{state | received: state.received + 1})

      {:done, _name} ->
        new_state = %{state | done: state.done + 1}

        if new_state.received == new_state.total and
             new_state.done == new_state.expected_workers do
          send(new_state.logger, {:log, "\nAll workers finished."})
          send(new_state.logger,
            {:log, "Total pulses received: #{new_state.received}/#{new_state.total}"}
          )
        else
          monitor_loop(new_state)
        end
    end
  end

  defp timestamp do
    {:ok, dt} = DateTime.now("Etc/UTC")
    dt |> DateTime.to_time() |> Time.truncate(:second) |> to_string()
  end
end

PulseMonitor.run()
