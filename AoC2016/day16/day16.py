state = '00101000101111010'
mem_pt1 = 272
mem_pt2 = 35651584

def dcurve(state, L):
    while len(state) < L:
        a = state
        b = a.translate(str.maketrans('01', '10'))[::-1]
        state = a + '0' + b
    return(state[:L])

def checksum(state):
    while len(state)%2 == 0:
        pairs = zip(state[::2],state[1::2])
        state = ''.join([str(int(i==j)) for i,j in pairs])
    return state 
    
def fill_and_checksum(state, L):
    state = dcurve(state, L)
    return checksum(state)


pt1 = fill_and_checksum(state, mem_pt1)

print('Part 1: ' + pt1)

# part 2
pt2 = fill_and_checksum(state, mem_pt2)

print('Part 2: ' + pt2)
