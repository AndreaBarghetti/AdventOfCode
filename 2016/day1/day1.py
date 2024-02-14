with open('input.txt') as file:
    for line in file:
        dirs = [l.strip() for l in line.split(',')]

# part 1
def follow_path(dirs):

    comp = {'N' : [0,1],
            'S' : [0,-1],
            'W' : [-1,0],
            'E' : [1,0]}

    ds = ['N','E','S','W']
    cur_dir = 0
    pos = [0,0]

    for dir in dirs:
        turn = dir[0]
        steps = int(dir[1:])

        if turn == 'R': cur_dir += 1 
        if turn == 'L': cur_dir -= 1 
        cur_dir = cur_dir%4

        pos[0] += comp[ds[cur_dir]][0] * steps
        pos[1] += comp[ds[cur_dir]][1] * steps

    return pos

pos = follow_path(dirs)

pt1_solution = abs(pos[0])+abs(pos[1])

print('Part1: '+str(pt1_solution))

# part 2

def follow_path2(dirs):

    comp = {'N' : [0,1],
            'S' : [0,-1],
            'W' : [-1,0],
            'E' : [1,0]}
    
    ds = ['N','E','S','W']
    
    pos = [0,0]
    cur_dir = 0
    visited = []
    
    for dir in dirs:
        turn = dir[0]
        steps = int(dir[1:])

        if turn == 'R': cur_dir += 1 
        if turn == 'L': cur_dir -= 1 
        cur_dir = cur_dir%4

        for _ in range(steps):
            pos[0] += comp[ds[cur_dir]][0]
            pos[1] += comp[ds[cur_dir]][1]

            if pos[:] in visited:
                return(pos)
            else: visited.append(pos[:])

pos = follow_path2(dirs)
pt2_solution = abs(pos[0])+abs(pos[1])

print('Part2: '+str(pt2_solution))