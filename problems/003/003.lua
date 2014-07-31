posix = require "posix"

local function solution()
  local factor = 1
  local n = 600851475143

  while true do
    factor = factor + 2

    while n % factor == 0 do
      n = n / factor
    end

    if factor * factor > n then
      return n
    elseif n == 1 then
      return factor
    end
  end
end

if #arg == 1 and arg[1] == "-a" then
  print(solution())
  return
end

io.stdout:setvbuf("line")
for line in io.lines() do
  local iters = tonumber(line)

  local start = { posix.clock_gettime("monotonic") }
  for i = 1,iters do
    solution()
  end
  local end_ = { posix.clock_gettime("monotonic") }

  local secs = end_[1] - start[1]
  local nsecs = end_[2] - start[2]
  print(secs * 1000000000 + nsecs)
end
