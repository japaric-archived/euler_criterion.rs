function is_palindrome(n)
  reversed, temp = 0, n

  while temp != 0
    reversed = 10 * reversed + temp % 10;
    temp = div(temp, 10)
  end

  reversed == n
end

function solution()
  max = 0

  for a in 100:999
    for b in 100:a
      p = a * b

      if p > max && is_palindrome(p)
        max = p
      end
    end
  end

  max
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
