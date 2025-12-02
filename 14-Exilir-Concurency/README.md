# Многопроцессный монитор пульсов без артефактов вывода
Эта программа на языке Elixir реализует надёжный многопроцессный монитор, в котором:
- несколько workers генерируют «pulse»-события c заданным интервалом;
- все рабочие процессы отправляют лог-сообщения в отдельный logger-процесс, поэтому нет смешивания вывода;
- основной процесс (monitor) собирает статистику и завершает работу, когда все события получены.
# Формат входного файла
Каждая непустая строка:
<pre>
name interval_ms count
</pre>
Пример:
<pre>
sensor_a 500 5
sensor_b 800 3
sensor_c 1200 4
</pre>
- name — имя датчика
- interval_ms — задержка между импульсами
- count — сколько импульсов должен отправить worker
# Пример вывода программы
<pre>
Using config file: pulses.txt
Spawned 3 workers, expect 12 pulses.

[12:14:01] pulse from sensor_a, remaining=4
[12:14:01] pulse from sensor_b, remaining=2
[12:14:02] pulse from sensor_c, remaining=3
...
[12:14:06] sensor_a finished.
...
All workers finished.
Total pulses received: 12/12
</pre>