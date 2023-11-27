import re

with open("Day6/input.txt") as file:
  input = file.read().rstrip()

def find_signal_start(signal, min_size):
  for i in range(len(signal)):
    substr = signal[i:(i+min_size)]
    if not bool(re.search("(.).*\\1",substr)):
      return i+min_size

# Part 1 ####
find_signal_start(input,4)

# Part 2 ####
find_signal_start(input,14)
