# By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
# What is the 10,001st prime number?

require "problem"

problem 7 do
  # Sieve of Eratosthenes
  code do 
    max = 1_000_000 # 10,001st prime is probably less than 1 million

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

    primes = [] of Int32

    sieve.each_with_index { |prime, number| primes << number if prime }

    primes[10_000] 
  end
  expect 104743
end
