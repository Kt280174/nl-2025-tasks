#!/usr/bin/env ruby

FILENAME = "/uploads/input.txt"

unless File.exist?(FILENAME)
  STDERR.puts "File #{FILENAME} not found"
  exit 1
end

lines = File.read(FILENAME).lines

stack = []
vars = {}            # biến
pc = 0               # program counter
loop_stack = []      # vị trí LOOP và số lần chạy
skip_mode = false    # dùng cho IF sai điều kiện
skip_until_end = 0

def get_value(vars, token)
  if token =~ /\A-?\d+\z/
    token.to_i
  elsif vars.key?(token)
    vars[token]
  else
    raise "Unknown value or variable: #{token}"
  end
end

while pc < lines.size
  raw = lines[pc]
  line_no = pc + 1
  pc += 1
  line = raw.strip

  next if line.empty? || line.start_with?("#")

  # Nếu đang skip (IF false) → bỏ qua đến END
  if skip_mode
    skip_until_end += 1 if line =~ /\AIF\b/
    skip_until_end -= 1 if line =~ /\AEND\b/

    if line =~ /\AEND\b/ && skip_until_end == 0
      skip_mode = false
    end
    next
  end

  case line

  # SET x 10
  when /\ASET\s+([A-Za-z_]\w*)\s+(-?\d+)\z/
    var, value = $1, $2.to_i
    vars[var] = value

  # PUSH x hoặc PUSH 10
  when /\APUSH\s+([A-Za-z_]\w*|-?\d+)\z/
    token = $1
    stack << get_value(vars, token)

  when /\AADD(?:\s+([A-Za-z_]\w*|-?\d+)\s+([A-Za-z_]\w*|-?\d+))?\z/
    if $1
      a = get_value(vars, $1)
      b = get_value(vars, $2)
      stack << (a + b)
    else
      raise "Not enough stack items for ADD" if stack.size < 2
      b = stack.pop
      a = stack.pop
      stack << (a + b)
    end

  when /\ASUB\z/
    raise "Not enough stack" if stack.size < 2
    b = stack.pop
    a = stack.pop
    stack << (a - b)

  when /\AMUL\z/
    raise "Not enough stack" if stack.size < 2
    b = stack.pop
    a = stack.pop
    stack << (a * b)

  when /\ADIV\z/
    raise "Not enough stack" if stack.size < 2
    b = stack.pop
    raise "Division by zero" if b == 0
    a = stack.pop
    stack << (a / b)

  when /\APRINT\z/
    puts stack.last

  when /\APOP\z/
    raise "POP on empty stack" if stack.empty?
    stack.pop

  when /\ADUP\z/
    raise "DUP on empty stack" if stack.empty?
    stack << stack.last

  # LOOP N
  when /\ALOOP\s+([A-Za-z_]\w*|-?\d+)\z/
    count = get_value(vars, $1)
    loop_stack << { start: pc, count: count }

  # END (kết thúc IF hoặc LOOP)
  when /\AEND\z/
    if !loop_stack.empty?
      frame = loop_stack.last
      frame[:count] -= 1
      if frame[:count] > 0
        pc = frame[:start]   # quay lại đầu LOOP
      else
        loop_stack.pop
      end
    end

  # IF condition
  when /\AIF\s+([A-Za-z_]\w*|-?\d+)\s*([><=]{1,2})\s*([A-Za-z_]\w*|-?\d+)\z/
    left  = get_value(vars, $1)
    op    = $2
    right = get_value(vars, $3)

    cond = case op
    when ">"  then left > right
    when "<"  then left < right
    when "==" then left == right
    else
      raise "Unknown operator #{op}"
    end

    unless cond
      skip_mode = true
      skip_until_end = 1
    end

  else
    STDERR.puts "Syntax error at line #{line_no}: #{line}"
    exit 1
  end
end
