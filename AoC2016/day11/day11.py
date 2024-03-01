import os
from functools import reduce
from collections import deque
from itertools import combinations

# script_dir = os.path.dirname(os.path.abspath(__file__))
# input_file_path = os.path.join(script_dir, 'input.txt')

gens = [1,1,1,3,3]
chips = [1,2,2,3,3]
elevator = 1

state = (gens, chips, elevator)

def hash_state(state):
    gens, chips, elevator = state
    return tuple(gens + chips + [elevator])
        
def is_valid_state(state):
    gens, chips, elevator = state
    for g,c in zip(gens, chips):
        if c != g and c in gens:
            return False
    return True
    
def is_done(state):
    gens, chips, elevator = state
    return all([l ==4 for l in gens+chips])
    
def get_next_states(state):
    gens, chips, elevator = state
    items = gens+chips
    next_states = []
    
    # Items available to move (index pairs of gens and chips on the same floor as the elevator)
    i_to_move = [i for i, g in enumerate(items) if g == elevator]
    i2_to_move = combinations(i_to_move, 2)
    
    # move only one thing
    for i in i_to_move:
        for dir in [+1,-1]:
            nitems = items.copy()
            if 1<= elevator + dir <=4:
                nelevator = elevator+dir
                nitems[i] = nelevator
                ngens, nchips = nitems[:len(chips)],nitems[len(chips):]
                nstate = (ngens, nchips, nelevator)
                if is_valid_state(nstate):
                    next_states.append(nstate)
    
    # move two things
    for i1,i2 in i2_to_move:
        for dir in [+1,-1]:
            nitems = items.copy()
            if 1 <= elevator + dir <= 4:
                nelevator = elevator+dir
                nitems[i1]=nitems[i2] = nelevator
                ngens, nchips = nitems[:len(chips)],nitems[len(chips):]
                nstate = (ngens, nchips, nelevator)
                if is_valid_state(nstate):
                    next_states.append(nstate)
            
    return next_states


def count_steps(state):
    steps=0
    q = deque([(state, steps)])
    seen = set(hash_state(state))
    
    while (not is_done(state)):
        state, steps = q.popleft()
        n_states = get_next_states(state)
        
        for s in n_states:
            if hash_state(s) in seen:
                continue
            seen.add(hash_state(s))
            q.append((s, steps+1))
        
    return steps


pt1 = count_steps(state) 
    
print('Part 1: ' + str(pt1))

# part 2
gens = gens+[1,1]
chips = chips + [1,1]
elevator = 1

state = (gens, chips, elevator)

pt2 = count_steps(state)
 
print('Part 2: ' + str(pt2))
