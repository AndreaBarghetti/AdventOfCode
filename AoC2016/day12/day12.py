import os

script_dir = os.path.dirname(os.path.abspath(__file__))
input_file_path = os.path.join(script_dir, 'input.txt')


with open(input_file_path) as file:
    bunnycode = file.read().splitlines()
    
    
# part 1
    
def run_code(bunnycode, 
             register = {'a':0, 'b':0, 'c':0, 'd':0}):

    i=0

    while i < len(bunnycode):
        
        f,*xy = bunnycode[i].split()
        
        if f == 'cpy':
            value = int(xy[0]) if xy[0].isdigit() else register[xy[0]]
            register[xy[1]] = value
        elif f == 'inc':
            register[xy[0]] += 1
        elif f == 'dec':
            register[xy[0]] -= 1
        elif f == 'jnz':
            value = int(xy[0]) if xy[0].isdigit() else register[xy[0]]
            if value != 0:
                i = i + int(xy[1])
                continue
        i +=1
    
    return(register)
    
pt1 = run_code(bunnycode)

print('Part 1:', str(pt1['a']))

# part 2
pt2 = run_code(bunnycode, register = {'a':0, 'b':0, 'c':1, 'd':0})

print('Part 2:', str(pt2['a']))
