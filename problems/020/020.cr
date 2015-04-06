# n! means n × (n − 1) × ... × 3 × 2 × 1
#
# For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
# and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
# 
# Find the sum of the digits in the number 100!

require "problem"
require "bignum"

problem 20 do
  code do
    fact = Bignum.new(1)
    2.upto(100) { |i| fact *= i }
    fact.digits.sum
  end
  expect 648
end
