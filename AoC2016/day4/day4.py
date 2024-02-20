import os
import numpy as np
import regex as re
from collections import Counter
import string

script_dir = os.path.dirname(os.path.abspath(__file__))
input_file_path = os.path.join(script_dir, 'input.txt')

# input_file_path = '2016/day4/input.txt'
# room = 'fubrjhqlf-edvnhw-dftxlvlwlrq-803[wjvzd]'

def rank_letters(string):
    letters = re.sub(r'[^A-Za-z]', '', string)
    counts = Counter(letters)
    sorted_counts = sorted(counts.items(), key=lambda x: (-x[1], x[0]))
    return sorted_counts

def get_checksum(room):
    checksum = re.search(r"\[(.*?)\]", room).group(1)
    return(checksum)

def get_sector_id(room):
    sector_id = re.search(r"-(\d+)\[", room).group(1)
    return(sector_id)

def get_name(room):
    name = re.search(r"([^\d]+)\d+", room).group(1)
    return(name)

def is_real_room(room):
    checksum = get_checksum(room)
    sector_id = get_sector_id(room)
    name = get_name(room)
    top5 = [l[0] for l in rank_letters(name)[0:5]]
    is_room = bool(np.prod([l in top5 for l in checksum]))
    if is_room: return(int(sector_id)) 
    else: return(0)

with open(input_file_path) as rooms:
    real=0
    for room in rooms:
        real += is_real_room(room)
    print('Part 1: ' + str(real))


# part 2
def rotate_letter(l, int):
    letters = string.ascii_lowercase
    if l not in letters: 
        return(l)
    pos = letters.find(l)
    npos = (pos + int) % 26
    return(letters[npos])

def decrypt_room(room):
    name = get_name(room).replace('-',' ')
    sector_id = int(get_sector_id(room))
    return(''.join([rotate_letter(l, sector_id) for l in name]))


with open(input_file_path) as rooms:
    for room in rooms:
        droom = decrypt_room(room)
        if 'northpole' in droom:
            print('Part 2: ' + get_sector_id(room))
            break