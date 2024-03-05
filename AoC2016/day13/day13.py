from collections import deque

NUMBER = 1350
START = (1,1)
END = (31,39)

def check(pos, num=NUMBER):
    x,y = pos
    res = x*x + 3*x + 2*x*y + y + y*y + num
    return(bin(res).count('1') %2 == 0)

def use_bfs(pos,end=None, max_steps=None):
    
    moves = ((0,1),(0,-1),(1,0),(-1,0))
    steps = 0
    q = deque([(pos,steps)])
    seen = set()
    
    while q:
        pos, steps = q.popleft()
        
        if pos == end: 
            return(steps)
        if max_steps is not None and steps > max_steps: 
            return(len(seen))
        
        seen.add(pos)
        
        for m in moves:
            npos = (pos[0]+m[0], pos[1]+m[1])
            if npos in seen:
                continue
            if any([v<0 for v in npos]):
                continue
            if not check(npos):
                continue
            
            q.append((npos,steps+1))

pt1 = use_bfs(START,END)

print('Part 1: '+ str(pt1))

# part 2
pt2 = use_bfs((1,1), max_steps=50)
print('Part 2: '+ str(pt2))

