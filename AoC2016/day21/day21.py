import os

script_dir = os.path.dirname(os.path.abspath(__file__))
input_file_path = os.path.join(script_dir, 'input.txt')

#read input
with open(input_file_path, 'r') as file:
    input_lines = [line.strip() for line in file.readlines()]

def parse_instruction(line):
    parts = line.split()
    if parts[0] == 'swap' and parts[1] == 'position':
        return ('swap_position', int(parts[2]), int(parts[5]))
    elif parts[0] == 'swap' and parts[1] == 'letter':
        return ('swap_letter', parts[2], parts[5])
    elif parts[0] == 'rotate' and (parts[1] == 'left' or parts[1] == 'right'):
        return ('rotate', parts[1], int(parts[2]))
    elif parts[0] == 'rotate' and parts[1] == 'based':
        return ('rotate_based', parts[6])
    elif parts[0] == 'reverse':
        return ('reverse', int(parts[2]), int(parts[4]))
    elif parts[0] == 'move':
        return ('move', int(parts[2]), int(parts[5]))
    else:
        raise ValueError(f"Unknown instruction: {line}")
    
instructions = [parse_instruction(line) for line in input_lines]

# part 1 
input = 'abcdefgh'

def swap_position(s, x, y):
    lst = list(s)
    lst[x], lst[y] = lst[y], lst[x]
    return ''.join(lst)

def swap_letter(s, a, b):
    return s.replace(a, '#').replace(b, a).replace('#', b)

def rotate(s, direction, steps):
    if direction == 'left':
        return s[steps:] + s[:steps]
    elif direction == 'right':
        return s[-steps:] + s[:-steps]
    else:
        raise ValueError(f"Unknown rotation direction: {direction}") 
    
def rotate_based(s, letter):
    index = s.index(letter)
    steps = 1 + index
    if index >= 4:
        steps += 1
    return rotate(s, 'right', steps % len(s))

def reverse(s, x, y):
    return s[:x] + s[x:y+1][::-1] + s[y+1:]

def move(s, x, y):
    char = s[x]
    s = s[:x] + s[x+1:]
    s = s[:y] + char + s[y:]
    return s

def scramble(s, instructions):
    for instruction in instructions:
        if instruction[0] == 'swap_position':   
            s = swap_position(s, instruction[1], instruction[2])
        elif instruction[0] == 'swap_letter':
            s = swap_letter(s, instruction[1], instruction[2])
        elif instruction[0] == 'rotate':
            s = rotate(s, instruction[1], instruction[2])
        elif instruction[0] == 'rotate_based':
            s = rotate_based(s, instruction[1])
        elif instruction[0] == 'reverse':
            s = reverse(s, instruction[1], instruction[2])
        elif instruction[0] == 'move':
            s = move(s, instruction[1], instruction[2])
    return s    

scrambled = scramble(input, instructions)

print("Part 1:", scrambled)

# part 2
scrambled_input = 'fbgdceah'

def reverse_rotate_based(s, letter):
    for i in range(len(s)):
        candidate = rotate(s, 'left', i)
        if rotate_based(candidate, letter) == s:
            return candidate
    raise ValueError("No valid reverse rotation found")

def unscramble(s, instructions):
    for instruction in instructions[::-1]:  
        if instruction[0] == 'swap_position':   
            s = swap_position(s, instruction[1], instruction[2])
        elif instruction[0] == 'swap_letter':
            s = swap_letter(s, instruction[1], instruction[2])
        elif instruction[0] == 'rotate':
            opposite_direction = 'left' if instruction[1] == 'right' else 'right'
            s = rotate(s, opposite_direction, instruction[2])
        elif instruction[0] == 'rotate_based':
            s = reverse_rotate_based(s, instruction[1])
        elif instruction[0] == 'reverse':
            s = reverse(s, instruction[1], instruction[2])
        elif instruction[0] == 'move':
            s = move(s, instruction[2], instruction[1])
    return s    

unscambled = unscramble(scrambled_input, instructions)

print("Part 2:", unscambled)