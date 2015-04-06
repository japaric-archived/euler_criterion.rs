# A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
# a^2 + b^2 = c^2
# For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
# There exists exactly one Pythagorean triplet for which a + b + c = 1000.
# Find the product abc.

require "problem"

# Euclid's formula
def euclid(sum)
  (1..sum).each do |m|
    (1..sum).each do |n|
      (1..sum).each do |k|
        if m > n
          a = k * (m ** 2 - n ** 2)
          b = k * (2 * m * n)
          c = k * (m ** 2 + n ** 2)
          if a + b + c == sum
            return a * b * c
          end
        end
      end
    end
  end
end

problem 9 do
  code do
    euclid 1000
  end
  expect 31875000
end