function solution()
  n = 600851475143

  factor = 1
  while true
    factor += 2

    while n % factor == 0
      n = div(n, factor)
    end

    if factor * factor > n
      return n
    elseif n == 1
      return factor
    end
  end
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
