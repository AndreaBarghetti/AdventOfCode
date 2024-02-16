import os
import numpy as np

script_dir = os.path.dirname(os.path.abspath(__file__))
input_file_path = os.path.join(script_dir, 'input.txt')

# part 1
triangles = np.loadtxt(input_file_path)

def count_triangles(triangles):
   possible = 0 
   for triangle in triangles:
      if max(triangle) < sum(triangle) - max(triangle):
         possible += 1
   return(possible)

print('Part1: ' + str(count_triangles(triangles)))

# part 2
triangles2 = triangles.transpose().reshape(-1).reshape(-1,3)

print('Part2: ' + str(count_triangles(triangles2)))