function solution()
  n = 2

  for i = 3:20
    n = lcm(n, i)
  end

  n
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
