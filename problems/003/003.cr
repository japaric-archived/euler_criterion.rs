# The prime factors of 13195 are 5, 7, 13 and 29.
# What is the largest prime factor of the number 600851475143?

require "problem"

def first_factor(x)
  (2_u64..x).each { |i| return i if x.divisible_by?(i) }
  return 0_u64
end

problem 3 do
  code do
    factors = [] of UInt64
    x = num = 600851475143_u64

    while x > 1
      factors << first_factor(x)
      x /= factors.last
    end

    factors.max
  end
  expect 6857
end
