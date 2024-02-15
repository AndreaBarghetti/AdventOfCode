import os
script_dir = os.path.dirname(os.path.abspath(__file__))
input_file_path = os.path.join(script_dir, 'input.txt')

with open(input_file_path) as file:
    instructions = []
    for line in file:
        instructions.append(line)

# part 1
keypad=[[1,2,3],[4,5,6],[7,8,9]]
start=[1,1]

def get_code(instructions, keypad, start):
    pos=start
    code=''
    for line in instructions:
        for l in line:

            npos = pos[:]

            if l == 'U': npos[0] = max(npos[0] - 1, 0)
            if l == 'D': npos[0] = min(npos[0] + 1, len(keypad) - 1)
            if l == 'L': npos[1] = max(npos[1] - 1, 0)
            if l == 'R': npos[1] = min(npos[1] + 1, len(keypad[npos[0]]) - 1)
            if keypad[npos[0]][npos[1]] != 0: pos = npos

        code = code + str(keypad[pos[0]][pos[1]])
    
    return(code)

print("Part1: " + get_code(instructions, keypad, start))

# part 2
keypad=[[0,0,1,0,0],[0,2,3,4,0],[5,6,7,8,9],[0,'A','B','C',0],[0,0,'D',0,0]]
start=[2,0]

print("Part2: " + get_code(instructions, keypad, start))

