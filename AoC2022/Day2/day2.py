with open('Day2/input.txt') as file:
    lines = file.read().strip().split("\n")


# PART 1

rps_dict={
  "A":"R","B":"P","C":"S",
  "X":"R","Y":"P","Z":"S"
  }
  
rps_point={"RR": "4", "RP": "8", "RS": "3",
           "PR": "1", "PP": "5", "PS": "9",
           "SR": "7", "SP": "2", "SS": "6"}
           
def get_score(game):
  for x, y in rps_dict.items():
    game = game.replace(x, y).replace(" ","")
  for x, y in rps_point.items():
    game = game.replace(x, y)
  return int(game)

scores = [get_score(line) for line in lines]

sum(scores)

# PART 2
rps_dict={
  "A":"R","B":"P","C":"S"
  }

strategy = {
  "RX":"RS","RY":"RR","RZ":"RP",
  "PX":"PR","PY":"PP","PZ":"PS",
  "SX":"SP","SY":"SS","SZ":"SR"}

def get_score2(game):
  for x, y in rps_dict.items():
    game = game.replace(x, y).replace(" ","")
  for x, y in strategy.items():
    game = game.replace(x, y)
  for x, y in rps_point.items():
    game = game.replace(x, y)
  return int(game)

scores = [get_score2(line) for line in lines]

sum(scores)
