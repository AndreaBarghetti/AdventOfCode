import os

script_dir = os.path.dirname(os.path.abspath(__file__))
input_file_path = os.path.join(script_dir, 'input.txt')

with open(input_file_path) as file:
    row = file.read().strip()
    
def next_row(row):
    nrow = ''
    row = "."+row+"."
    for l, r in zip(row,row[2:]):
        if l==r: nrow += "."
        else: nrow += "^"
    return nrow

def count_tiles(row, n):
    traps = row.count('.')
    for _ in range(n-1):
        row = next_row(row)
        traps += row.count('.')
    return traps

# part 1
pt1 = count_tiles(row,40)

print('Part 1: ' + str(pt1))

# part 2
pt2 = count_tiles(row, 400000)

print('Part 2: ' + str(pt2))