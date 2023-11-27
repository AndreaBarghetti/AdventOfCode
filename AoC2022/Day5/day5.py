with open('Day5/input.txt') as file:
    input = file.read().strip().split("\n")

import numpy as np
import re


def parse_input(input):

  sep_line = int(np.where(np.array(input)=="")[0])
  input_stacks = input[0:(sep_line-1)]
  input_procedures = input[(sep_line+1):]

  stacks = read_stacks(input_stacks)
  procedures = read_procedures(input_procedures)

  list(stacks=stacks,
       procedures=procedures)


def clean_line(line):
    cline = re.sub("[\[\]]","",line.replace("   ","_").replace(" ",""))
    return cline

def read_stacks(input_stacks):
  pass

def read_procedures(input_procedures):
  pass


# WIP
ostacks = [*map(clean_line, input_stacks)]
