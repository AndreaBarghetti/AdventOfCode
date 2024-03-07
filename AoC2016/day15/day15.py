import os
import re

script_dir = os.path.dirname(os.path.abspath(__file__))
input_file_path = os.path.join(script_dir, 'input.txt')


with open(input_file_path) as file:
    discs=[]
    for l in file.read().splitlines():
        d_dist, d_size, _, d_start = (int(x) for x in re.findall('\d+', l))
        discs.append((d_dist, d_size, d_start))
        
def try_1disc(time, disc):
    d_dist, d_size, d_start = disc
    t_reach = time + d_dist
    pos = d_start + t_reach
    pos = pos % d_size
    return pos == 0


def try_discs(time, discs):
    for disc in discs:
        if not try_1disc(time, disc):
            return(False)
    return True
       
def find_time(discs):
    t=0
    while True:
        if try_discs(t, discs): 
            return t
        t += 1
        
pt1 = find_time(discs)

print('part 1: ' + str(pt1))

# part 2
discs2 = discs[:]

discs2.append((7,11,0))

pt2 = find_time(discs2)

print('part 2: ' + str(pt2))