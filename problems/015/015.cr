# Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
# How many such routes are there through a 20×20 grid?

# Start at top-left and move to bottom-right -> must move right 20 times and down 20 times. 
# For example: RRDDRRDDRRDDRRDDRRDDRRDDRRDDRRDDRRDDRRDD
# Solution: number of unique combinations of 20 R's and 20 D's.
#   => number of way to choose 20 spots out of 40 (if we choose all 20 R's, then the D's are the other 20)
#   => 40 choose 20 (binomial coefficient)

require "problem"

problem 15 do
  code do
    grid = 20_u64
    num_paths = 1_u64

    # Multiplicative Formula (binomial coefficient)
    grid.times do |i|
      num_paths *= (2 * grid) - i
      num_paths /= i + 1
    end

    num_paths
  end
  expect 137846528820_u64
end
