import os

script_dir = os.path.dirname(os.path.abspath(__file__))
input_file_path = os.path.join(script_dir, 'input.txt')


with open(input_file_path, 'r') as file:
    df_out_lines = file.readlines()[2:]  # Skip the first two header lines

def parse_node(line):
    parts = line.split()
    name = parts[0]
    size = int(parts[1][:-1])  # Remove 'T' and convert to int
    used = int(parts[2][:-1])  # Remove 'T' and convert to int
    avail = int(parts[3][:-1])  # Remove 'T' and convert to int
    use_percent = int(parts[4][:-1])  # Remove '%' and convert to int
    x, y = int(name.split('-')[1][1:]), int(name.split('-')[2][1:])
    return {
        'name': name,
        'x': x,
        'y': y,
        'size': size,
        'used': used,
        'avail': avail,
        'use_percent': use_percent
    }

nodes = [parse_node(line) for line in df_out_lines]

# part 1
viable_pairs = 0
for a in nodes:
    for b in nodes:
        if a != b and a['used'] > 0 and a['used'] <= b['avail']:
            viable_pairs += 1

print( 'part 1:', viable_pairs)


# part 2
grid = {}
for node in nodes:
    grid[(node['x'], node['y'])] = node 

max_x = max(x for x, y in grid.keys())
max_y = max(y for x, y in grid.keys())

# Identify the empty node
for (x, y), node in grid.items():
    if node['used'] == 0:
        empty_node = (x, y)
        break

