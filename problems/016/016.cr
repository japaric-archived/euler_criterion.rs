# 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
# What is the sum of the digits of the number 2^1000?

require "problem"
require "bignum"

problem 16 do
  code do
    bignum = Bignum.new(1)
    1000.times { bignum *= 2 }
    bignum.digits.sum
  end
  expect 1366
end
