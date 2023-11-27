with open('Day4/input.txt') as file:
    lines = file.read().strip()

lines = lines.split("\n")
    
# PART 1
import re

def between(test, min, max):
  res = [min <= t <= max for t in test]
  return res

def check_within(line):
  cvals = re.findall('\\d+',line)
  vals = [int(x) for x in cvals]
  overlap = all(between(vals[0:2], vals[2],vals[3])) | all(between(vals[2:], vals[0],vals[1]))
  return overlap

sum([*map(check_within, lines)])

# PART 2

def check_within2(line):
  cvals = re.findall('\\d+',line)
  vals = [int(x) for x in cvals]
  overlap = any(between(vals[0:2], vals[2],vals[3])) | any(between(vals[2:], vals[0],vals[1]))
  return overlap

sum([*map(check_within2, lines)])

