import os

script_dir = os.path.dirname(os.path.abspath(__file__))
input_file_path = os.path.join(script_dir, 'input.txt')

# read the input file
with open(input_file_path, 'r') as f:
    input_data = f.read().strip().splitlines()

# parse the input data  into a list of tuples
blocked_ranges = []
for line in input_data:
    start, end = map(int, line.split("-"))
    blocked_ranges.append((start, end))
    
# sort the ranges by their starting point
blocked_ranges.sort()

# merge overlapping ranges
merged_ranges = []
for start, end in blocked_ranges:
    if not merged_ranges or start > merged_ranges[-1][1] + 1:
        merged_ranges.append((start, end))
    else:
        merged_ranges[-1] = (merged_ranges[-1][0], max(merged_ranges[-1][1], end))  # merge ranges

# Part 1: Find the lowest-valued IP that is not blocked
lowest_unblocked_ip = 0
for start, end in merged_ranges:
    if lowest_unblocked_ip < start:
        break
    lowest_unblocked_ip = end + 1

print("Part 1:", lowest_unblocked_ip)

# Part 2: How many IPs are allowed by the blacklist?
allowed_ip_count = 0
current_ip = 0
for start, end in merged_ranges:
    if current_ip < start:
        allowed_ip_count += start - current_ip
    current_ip = max(current_ip, end + 1)

allowed_ip_count += 4294967296 - current_ip # account for IPs after the last blocked range

print("Part 2:", allowed_ip_count)
