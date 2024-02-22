import os
import numpy as np

script_dir = os.path.dirname(os.path.abspath(__file__))
input_file_path = os.path.join(script_dir, 'input.txt')

# part 1

# realized later this already exists: np.roll
def rotate(x, n):
    n = n % len(x)
    return np.concatenate((x[-n:], x[:-n]))

def read_instr(row):
    tokens = row.split()
    if 'rect' in row:
        a, b = map(int, tokens[1].split('x'))
        return ('rect', a, b)
    if 'rotate' in row:
        axis = tokens[2].split('=')[0]
        i = int(tokens[2].split('=')[1])
        n = int(tokens[4])
        return ('rotate', axis, i, n)

class Screen:
    def __init__(self, rows=50, cols=50):
        self.screen = np.full((rows, cols), 0)
    
    def rect(self, a, b):
        self.screen[0:b, 0:a] = 1
        
    def rotate(self, axis, i, n):
        if axis == 'x':
            self.screen[:,i] = rotate(self.screen[:,i], n)
        
        if axis == 'y':
             self.screen[i,:] = rotate(self.screen[i,:], n)
          
    def __str__(self):
        screen_str = ""
        for row in self.screen:
            row_str = ''.join('#' if cell == 1 else ' ' for cell in row)
            screen_str += row_str + "\n"
        # could have been:
        # screen_str = '\n'.join(''.join('#' if p else ' ' for p in line) for line in self.screen)
        return screen_str
    
    def run_code(self, input_file_path):
        with open(input_file_path) as file:
            for row in file:
                instr = read_instr(row)
                
                if instr[0]=='rect':
                    self.rect(instr[1], instr[2])
                if instr[0]=='rotate':
                    self.rotate(instr[1], instr[2], instr[3])

screen = Screen()

screen.run_code(input_file_path)
                        
pt1 = np.sum(screen.screen==1)      

print('Part 1: ' + str(pt1))

# part 2
screen = Screen(6,50)
screen.run_code(input_file_path)

print('Part 2: \n')
print(screen)