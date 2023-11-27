elfs = list()
elfs.append(0)

with open('Day1/input.txt') as file:
    input = file.read().split("\n")

for value in input:
    if value == '':
        elfs.append(0)
    else:
        elfs[len(elfs)-1] += int(value)

max(elfs)

# part2 ####
elfs.sort(reverse=True)
sum(elfs[0:3])
