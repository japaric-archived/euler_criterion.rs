# The following iterative sequence is defined for the set of positive integers:
#   n → n/2 (n is even)
#   n → 3n + 1 (n is odd)
# Using the rule above and starting with 13, we generate the following sequence:
#   13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
# It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
# Which starting number, under one million, produces the longest chain?
# NOTE: Once the chain starts the terms are allowed to go above one million.

require "problem"

def perform_chain(lookup, num)
  if !lookup.has_key?(num)
    chain_len = if num.even?
      perform_chain(lookup, num / 2) + 1
    else
      perform_chain(lookup, 3_u64 * num + 1) + 1
    end
    lookup[num] = chain_len
  end
  return lookup[num]
end

problem 14 do
  code do
    chain_lookup = {1_u64 => 1}

    (2_u64..1_000_000).each { |i| perform_chain(chain_lookup, i) }

    longest_chain_key = 1
    chain_lookup.each { |key, value| longest_chain_key = key if value > chain_lookup[longest_chain_key] }

    longest_chain_key
  end
  expect 837799
end
