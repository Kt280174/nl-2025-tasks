local filename = "input_lua.txt"
local file = io.open(filename, "r")

if not file then
    print("Не удалось открыть input.txt")
    os.exit(1)
end

local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

local function get_indent(line)
    local _, count = line:find("^%s*")
    return math.floor((count or 0)/ 2)
end

local function compute_depth(index, current_indent)
    local max_depth = 1
    local i = index + 1
    
    while i <= #lines do
        local indent = get_indent(lines[i])
        if indent <= current_indent then
            break
        end 
        local depth, consumed = compute_depth(i, indent)
        if depth + 1 > max_depth then
            max_depth = depth + 1
        end
        i = consumed + 1
    end
    
    return max_depth, i - 1
end

local depth, _ = compute_depth(1, get_indent(lines[1]))

print("Max depth: " ..  depth)