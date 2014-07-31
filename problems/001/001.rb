def solution
  sum = 0

  (0..999).each do |i|
    sum += i if i % 3 == 0 || i % 5 == 0
  end

  sum
end

# XXX Shorter but 60% slower
#def solution
  #(0..999).select { |x| x % 3 == 0 || x % 5 == 0 }.inject(:+)
#end

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
