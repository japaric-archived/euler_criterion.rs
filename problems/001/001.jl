function solution()
  sum = 0

  for x = 1:999
    if x % 3 == 0 || x % 5 == 0
      sum += x
    end
  end

  sum
end

# XXX Shorter, but 100%+ slower. std_dev is way higher as well
#=function solution()=#
  #=sum([x % 3 == 0 || x % 5 == 0 ? x : 0 for x = 0:999])=#
#=end=#

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
