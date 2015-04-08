def solution
  sum = 0

  (0..999).each do |i|
    sum += i if i % 3 == 0 || i % 5 == 0
  end

  sum
end

if ARGV.length == 1 && ARGV[0] == "-a"
  puts solution
  exit
end
