PERIMETER = 1000

function solution()
  for c = div(PERIMETER, 3) + 1:div(PERIMETER, 2) - 1
    for b = div(PERIMETER - c, 2) + 1:c - 1
      a = PERIMETER - b - c

      if a * a + b * b == c * c
        return a * b * c
      end
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
