from hashlib import md5
import regex as re

salt = 'ahsbgdzn'

def make_hex(salt, i, key_stretching=0):
    s = md5((salt+str(i)).encode()).hexdigest()
    
    for _ in range(key_stretching):
            s = md5(s.encode()).hexdigest()
    
    return s
    
def get_triplet(s):
    r = re.search(r'(.)\1\1', s)
    if bool(r): return r.group()[0]
    else: return False

def has_quintlet(s,l):
    return l*5 in s

def find_keys(salt, key_stretching=0):
    
    keys = []
    triplets = {}

    i = 0

    while len(keys) <64 or i < max(keys) + 1000:
        
        s = make_hex(salt, i, key_stretching)
    
        for ti, tl in triplets.items():
            if tl*5 in s and ti >= i - 1000:
                keys.append(ti)
        
        t = get_triplet(s)
    
        if t:
            triplets[i] = t 
    
        i += 1
    
    keys.sort()
    return keys

keys = find_keys(salt)

pt1 = keys[63]

print('Part 1: ' + str(pt1))

# part 2

pt2 = find_keys(salt, key_stretching=2016)[63]

print('Part 2: ' + str(pt2))