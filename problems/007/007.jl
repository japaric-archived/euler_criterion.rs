function solution()
  i, map, n, q = 0, Dict(), 10000, 1

  while true
    q += 1
    p = pop!(map, q, 0)

    if p == 0
      map[q * q] = q

      if i == n
        return q
      else
        i += 1
      end
    else
      x = p + q

      while get(map, x, 0) != 0
        x += p
      end

      map[x] = p
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
