import os
import numpy as np
from collections import Counter

script_dir = os.path.dirname(os.path.abspath(__file__))
input_file_path = os.path.join(script_dir, 'input.txt')

# part 1
message = np.loadtxt(input_file_path, dtype=str)    

mat = np.array([list(m) for m in message])

def get_most_common(x):
    mc = Counter(x).most_common(1)[0][0]
    return(mc)

pt1 = ''.join(np.apply_along_axis(get_most_common, 0, mat))

print('Part 1: ' + pt1)

# part 2
def get_least_common(x):
    lc = Counter(x).most_common()[-1][0]
    return(lc)

pt2 = ''.join(np.apply_along_axis(get_least_common, 0, mat))

print('Part 2: ' + pt2)