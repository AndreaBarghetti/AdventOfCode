import os
import numpy as np
import regex as re

script_dir = os.path.dirname(os.path.abspath(__file__))
input_file_path = os.path.join(script_dir, 'input.txt')

with open(input_file_path) as file:
    ips = file.read().splitlines()

# part 1    
def get_supernet(ip):
    sn = ' '.join(re.split(r'\[.*?\]', ip))
    return sn

def get_hypernet(ip):
    pattern = r'\[(.*?)\]'
    hn = ' '.join(re.findall(pattern, ip))
    return hn

def has_abba(string):
    pattern = r'(.)((?!\1).)\2\1'
    return re.search(pattern, string) is not None

def support_tls(ip):
    sn = get_supernet(ip)
    if not has_abba(sn): return False    
    hn = get_hypernet(ip)
    return not has_abba(hn)

pt1 = sum([support_tls(ip) for ip in ips])

print('Part 1: ' + str(pt1))

# part 2
def get_abas(s):
    matches = []
    for i in range(len(s) - 2): 
        substring = s[i:i+3]
        
        if substring[0] == substring[2] and substring[0] != substring[1] and substring[1] != ' ':
            matches.append(substring)
    return matches

def babit(aba):
    return(aba[1]+aba[0]+aba[1])

def support_ssl(ip):
    sn = get_supernet(ip)
    hn = get_hypernet(ip)
    abas = get_abas(sn)
    return any([babit(aba) in hn for aba in abas])
     
pt2 = sum([support_ssl(ip) for ip in ips])

print('Part 2: ' + str(pt2))
