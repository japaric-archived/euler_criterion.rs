function solution()
  sum, curr, next = 0, 1, 2

  while curr < 4000000
    if curr % 2 == 0
      sum += curr
    end

    temp = next
    next += curr
    curr = temp
  end

  sum
end

if length(ARGS) == 1 && ARGS[1] == "-a"
  println(solution())
  exit(0)
end

for line in eachline(STDIN)
  iters = int(line)

  start = time_ns()
  for i in 1:iters
    solution()
  end
  end_ = time_ns()

  println(end_ - start)
end
