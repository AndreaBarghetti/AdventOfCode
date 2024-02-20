import hashlib
import os

script_dir = os.path.dirname(os.path.abspath(__file__))
input_file_path = os.path.join(script_dir, 'input.txt')

with open(input_file_path) as file:
    door = file.read()

def get_md5(string):
    input_bytes = string.encode('utf-8')
    hash_object = hashlib.md5(input_bytes)
    md5_hash = hash_object.hexdigest()
    return(md5_hash)

def get_pw(door):
    pw=''
    code = 0
    while len(pw)<8:
        string = door + str(code)
        md5 = get_md5(string)
        if md5[0:5] == '00000':
            pw = pw + str(md5[5])
        code += 1
    return(pw)

print('Part 1: ' + str(get_pw(door)))

# part 2

def get_pw2(door):
    pw = ['_','_','_','_','_','_','_','_']
    code = 0
    while '_' in pw:
        string = door + str(code)
        md5 = get_md5(string)
        if md5[0:5] == '00000':
            if md5[5] in str('01234567'):
                if pw[int(md5[5])] == '_':
                    pw[int(md5[5])] = str(md5[6])
        code += 1
    return(''.join(pw))

print('Part 2: ' + str(get_pw2(door)))

