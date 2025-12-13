-- ===============================
-- Shared buffer
-- ===============================
local buffer = {}
local BUFFER_SIZE = 4

-- ===============================
-- Producer: read from file
-- ===============================
function producer(filename)
    local file = io.open(filename, "r")
    if not file then
        error("Cannot open file: " .. filename)
    end

    for line in file:lines() do
        while #buffer >= BUFFER_SIZE do
            print("[Producer] Buffer full, waiting...")
            coroutine.yield()
        end

        table.insert(buffer, line)
        print("[Producer] Produced:", line)

        coroutine.yield()
    end

    file:close()
    print("[Producer] Finished reading file")
end

-- ===============================
-- Consumer
-- ===============================
function consumer(id)
    while true do
        while #buffer == 0 do
            if producer_done then
                print("[Consumer " .. id .. "] No more tasks, exiting")
                return
            end
            print("[Consumer " .. id .. "] Buffer empty, waiting...")
            coroutine.yield()
        end

        local task = table.remove(buffer, 1)
        print("[Consumer " .. id .. "] Processing:", task)

        coroutine.yield()
    end
end

-- ===============================
-- Global flag
-- ===============================
producer_done = false

-- ===============================
-- Create coroutines
-- ===============================
local prod = coroutine.create(function()
    producer("input.txt")
    producer_done = true
end)

local cons1 = coroutine.create(function() consumer(1) end)
local cons2 = coroutine.create(function() consumer(2) end)

-- ===============================
-- Scheduler (round-robin)
-- ===============================
local turn = 1

while coroutine.status(prod) ~= "dead"
   or coroutine.status(cons1) ~= "dead"
   or coroutine.status(cons2) ~= "dead" do

    if coroutine.status(prod) ~= "dead" then
        coroutine.resume(prod)
    end

    if turn == 1 then
        if coroutine.status(cons1) ~= "dead" then coroutine.resume(cons1) end
        if coroutine.status(cons2) ~= "dead" then coroutine.resume(cons2) end
    else
        if coroutine.status(cons2) ~= "dead" then coroutine.resume(cons2) end
        if coroutine.status(cons1) ~= "dead" then coroutine.resume(cons1) end
    end

    turn = 3 - turn
end


print("All tasks processed.")
