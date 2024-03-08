from hashlib import md5
from collections import deque

PASSCODE = 'edjrjqaa'

pos = [1,1]
path=''
state = (path, pos)

def check_doors(state):
    path, pos = state
    code = PASSCODE + path
    s = md5(code.encode()).hexdigest()[:4]
    doors = [l in ['b','c','d','e','f'] for l in s]
    doors = dict(zip(['U','D','L','R'], doors))
    if pos[1]==1:
        doors['L']=False
    if pos[1]==4:
        doors['R']=False
    if pos[0]==1:
        doors['U']=False
    if pos[0]==4:
        doors['D']=False
    return doors

def next_states(state):
    
    moves = {'U': [-1,0],
         'D': [1,0],
         'L': [0,-1],
         'R': [0,1]}
    
    path, pos = state
    next_states = []
    doors = check_doors(state)
    for dir, lock in doors.items():
        if lock:
            npath = path+dir
            npos = pos[:]
            npos[0] += moves[dir][0]
            npos[1] += moves[dir][1]
            nstate = (npath,npos)
            next_states.append(nstate)
    return next_states
            
def find_vault(state):
    
    q = deque([state])
    
    paths_to_vault = []
    shortest=False
    longest = 0
    
    while q:
        state = q.popleft()
        nstates = next_states(state)
        for s in nstates:
            if s[1]==[4,4]:
                paths_to_vault.append(s[0])
                longest = max(longest,len(s[0]))
                if shortest: continue
                shortest = s[0]
            q.append(s)
    
    return shortest, longest

pt12 = find_vault(state)

print('Part 1: '+ str(pt12[0]))

# part2
print('Part 2: '+ str(pt12[1]))