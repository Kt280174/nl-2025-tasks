defmodule SimpleServer do
  def start(port) do
    {:ok, socket} =
      :gen_tcp.listen(port, [:binary, packet: :line, active: false, reuseaddr: true])

    IO.puts("Server started on port #{port}")

    loop_accept(socket)
  end

  defp loop_accept(listen_socket) do
    {:ok, client_socket} = :gen_tcp.accept(listen_socket)

    Task.start(fn -> handle_client(client_socket) end)

    loop_accept(listen_socket)
  end

  defp handle_client(socket) do
    case :gen_tcp.recv(socket, 0) do
      {:ok, data} ->
        trimmed = String.trim(data)
        response = "RECEIVED: #{trimmed}\n"
        :gen_tcp.send(socket, response)
        handle_client(socket)

      {:error, _} ->
        :gen_tcp.close(socket)
    end
  end
end

defmodule Client do
  def send_messages(port, lines) do
    {:ok, socket} =
      :gen_tcp.connect('localhost', port, [:binary, packet: :line, active: false])

    Enum.each(lines, fn line ->
      msg = String.trim(line) <> "\n"
      IO.puts("Client: #{String.trim(line)}")

      :gen_tcp.send(socket, msg)

      {:ok, reply} = :gen_tcp.recv(socket, 0)
      IO.puts("Server: #{String.trim(reply)}")
    end)

    :gen_tcp.close(socket)
  end
end

# -------------------------
# Главная программа
# -------------------------
port = 4040

Task.start(fn -> SimpleServer.start(port) end)

# небольшой sleep для запуска сервера
Process.sleep(200)

# читаем input.txt
lines =
  case File.read("/uploads/input.txt") do
    {:ok, content} -> String.split(content, "\n", trim: true)
    _ -> []
  end

Client.send_messages(port, lines)
