import os
from collections import defaultdict

script_dir = os.path.dirname(os.path.abspath(__file__))
input_file_path = os.path.join(script_dir, 'input.txt')


# part 1

with open(input_file_path) as f:
    instructions = f.read().splitlines()

class Bot:
    def __init__(self, id, rules=None, values=None):
        self.id = id
        self.rules = rules if rules is not None else []
        self.values = values if values is not None else []
        self.type = id[0:3]
    
    def get(self, value):
        self.values.append(value)
        self.values.sort()
        
        if (self.values == [17,61]):
                print('Part 1: ')
                print(self)
                print('\n')
        
        if len(self.values) == 2:
            self.give(bots)
    
    def give(self, bots):
        
        if (len(self.values)==2 and self.type == 'bot'):
            
            low, high = self.values
            self.values = []
            
            if self.rules[0] not in bots.keys():
                bots[self.rules[0]] = Bot(id=self.rules[0])
            if self.rules[1] not in bots.keys():
                bots[self.rules[1]] = Bot(id=self.rules[1])
            
            bots[self.rules[0]].get(low)
            bots[self.rules[1]].get(high)
            
            bots[self.rules[0]].give(bots)
            bots[self.rules[1]].give(bots)
            
    def __str__(self):
        return (self.id + ":" + str(self.values))
        
def read_instr(line):
    
    splt = line.split()
    
    if line.startswith('bot'):
        from_bot, low_to, high_to = ''.join(splt[0:2]), ''.join(splt[5:7]), ''.join(splt[-2:])
        return("bot", from_bot, low_to, high_to)
    
    if line.startswith('value'):
        value, to_bot = int(splt[1]), ''.join(splt[-2:])
        return("value", value, to_bot)
        
        
bots = dict()

for line in instructions:
    
    instr = read_instr(line)
    
    if instr[0] == 'bot':
        
        _, from_bot, low_to, high_to = instr
        
        if from_bot not in bots.keys():
            bots[from_bot] = Bot(id=from_bot, rules=[low_to, high_to])

for line in instructions:
    instr = read_instr(line)
    if instr[0] == 'value':
        
        _, value, to_bot = instr
        
        if to_bot not in bots.keys():
              bots[to_bot] = Bot(id=to_bot)
              
        bots[to_bot].get(value)

# part 2
a,b,c = [bots[name].values[0] for name in ['output0','output1','output2']]

pt2 = a * b * c

print('Part 2: ' + str(pt2))