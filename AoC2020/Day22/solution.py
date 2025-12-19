import os

# read input
input_file = os.path.join(os.path.dirname(__file__), "input.txt")

with open(input_file) as f:
    lines = f.readlines()
# parse input
player1 = []
player2 = []
current_player = None
for line in lines:
    line = line.strip()
    if line == "Player 1:":
        current_player = player1
    elif line == "Player 2:":
        current_player = player2
    elif line:
        current_player.append(int(line))
        
#part 1
def play_game(p1, p2):
    p1 = p1.copy()
    p2 = p2.copy()
    while p1 and p2:
        c1 = p1.pop(0)
        c2 = p2.pop(0)
        if c1 > c2:
            p1.extend([c1, c2])
        else:
            p2.extend([c2, c1])
    return p1 if p1 else p2

winner = play_game(player1, player2)

def calculate_score(deck):
    score = 0
    for i, card in enumerate(reversed(deck)):
        score += (i + 1) * card
    return score

print("Part 1:", calculate_score(winner))

#part 2
def play_recursive_game(p1, p2):
    p1 = p1.copy()
    p2 = p2.copy()
    previous_rounds = set()
    
    while p1 and p2:
        current_state = (tuple(p1), tuple(p2))
        if current_state in previous_rounds:
            return 1, p1
        previous_rounds.add(current_state)
        
        c1 = p1.pop(0)
        c2 = p2.pop(0)
        
        if len(p1) >= c1 and len(p2) >= c2:
            winner, _ = play_recursive_game(p1[:c1], p2[:c2])
        else:
            winner = 1 if c1 > c2 else 2
            
        if winner == 1:
            p1.extend([c1, c2])
        else:
            p2.extend([c2, c1])
    
    return (1, p1) if p1 else (2, p2)

_, winner = play_recursive_game(player1, player2)
print("Part 2:", calculate_score(winner))

