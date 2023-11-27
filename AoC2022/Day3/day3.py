with open('Day3/input.txt') as file:
     lines = [line.rstrip() for line in file]

# PART 1

def get_common(sack): 
  mid = int(len(sack)/2)
  c1 = sack[0:mid]
  c2 =sack[mid:]
  common = set(c1).intersection(c2)
  return "".join(common)

commons = [*map(get_common, lines)]

import string
import numpy as np

scores = string.ascii_lowercase+string.ascii_uppercase

def get_score(letter):
  score = scores.find(letter)+1
  return score

sum([*map(get_score, commons)])

# PART2

groups_f = [*range(0,int(len(lines)),3)]
groups = [lines[i:(i+3)] for i in groups_f]

def get_common2(group):
  common = set(group[0]).intersection(group[1]).intersection(group[2])
  return "".join(common)

commons2 = [get_common2(group) for group in groups]

sum([*map(get_score, commons2)])
