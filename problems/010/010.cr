# The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
# Find the sum of all the primes below two million.

require "problem"

problem 10 do
  # Sieve of Eratosthenes
  code do
    max = 2_000_000 # 10,001st prime is probably less than 1 million

    sieve = Array.new(max, true)
    sieve[0] = false
    sieve[1] = false

    (2...max).each do |i|
      if sieve[i]
        (2 * i).step(max - 1, i) do |j|
          sieve[j] = false
        end
      end
    end

    sum = 0_u64

    sieve.each_with_index { |prime, number| sum += number if prime }

    sum 
  end
  expect 142913828922_u64
end
