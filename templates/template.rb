def solution
  # SOLUTION GOES HERE
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
