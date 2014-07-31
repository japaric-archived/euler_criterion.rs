def solution
  sum, curr, next_ = 0, 1, 2

  while curr < 4_000_000
    curr.even? && sum += curr

    temp = next_
    next_ += curr
    curr = temp
  end

  sum
end

if ARGV.length == 1 && ARGV[0] == '-a'
  puts solution
  exit
end

STDOUT.sync = true
STDIN.each do |line|
  iters = line.to_i

  start = Process.clock_gettime(Process::CLOCK_MONOTONIC)
  (1..iters).each do ||
    solution
  end
  end_ = Process.clock_gettime(Process::CLOCK_MONOTONIC)

  elapsed = ((end_ - start) * 1e9).to_i

  puts elapsed
end
