import os
import regex as re

script_dir = os.path.dirname(os.path.abspath(__file__))
input_file_path = os.path.join(script_dir, 'input.txt')

with open(input_file_path) as file:
    code = file.read().splitlines() 
code = code[0]

# part 1
def decompress_code(code):
    output = 0
    i = 0

    while i < len(code):
        if code[i] == '(':
            expr = re.search(r'\((.*?)\)', code[i:]).group(1)
            rep = [int(x) for x in expr.split("x")]
            output = output + rep[0]*rep[1]
            i = i+len(expr)+2+rep[0]
            
        else:
            output += 1
            i += 1
    return(output)


pt1 = decompress_code(code)

print("Part {}: {}".format(1,pt1))


# part 2
def decompress_code_v2(code):
    
    if "(" not in code:
        return len(code)
    
    ob = code.index("(")
    cb = code.index(")")
    
    L, T = (int(x) for x in code[ob+1:cb].split("x"))
    
    count = decompress_code_v2(code[cb+1:cb+1+L])
    
    output = ob + T * count + decompress_code_v2(code[cb+1+L:])

    return(output)


pt2 = decompress_code_v2(code)

print("Part {}: {}".format(2,pt2))