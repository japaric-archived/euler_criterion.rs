def solution
  n = 600_851_475_143

  (3..n).step(2) do |factor|
    n /= factor while n % factor == 0

    if factor * factor > n
      return n
    elsif n == 1
      return factor
    end
  end
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
