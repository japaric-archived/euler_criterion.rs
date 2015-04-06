# A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
# Find the largest palindrome made from the product of two 3-digit numbers.

require "problem"

problem 4 do
  code do
    palindromes = [] of Int32

    (100..999).each do |i|
      (i..999).each do |j|
        num = i * j
        palindromes << num if num.to_s == num.to_s.reverse
      end
    end

    palindromes.max
  end
  expect 906609
end
