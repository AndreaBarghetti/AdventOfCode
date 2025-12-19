n = 3001330

# read about Josephus problem
# don't understand the solution, but whatever

# part 1
def josephus_problem(n):
    highest_power_of_2 = 1
    while highest_power_of_2 * 2 <= n:
        highest_power_of_2 *= 2
    
    l = n - highest_power_of_2
    return 2 * l + 1

josephus_problem(n)

# part 2  
import math

def josephus_problem2(n):
  p = 3**math.floor(math.log(n) / math.log(3))
  return n if n == p else n - p + max(0, n - 2 * p)

josephus_problem2(n)