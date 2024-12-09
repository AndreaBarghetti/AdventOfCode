library(tidyverse)
input <- read_lines("AoC2024/Day9/input.txt")

parse = function(input) {
  
  t = str_split(input, "", simplify = T)
  blocks = t[seq(from = 1, to = length(t), by = 2)] %>% as.integer()
  spaces = t[seq(from = 2, to = length(t), by = 2)] %>% as.integer()
  
  list(blocks = blocks,
       spaces = spaces)
  
}

# Part 1 ####
diskmap = parse(input)

print_diskmap = function(diskmap) {
  pmap(list(seq_along(diskmap$blocks) - 1,
            diskmap$blocks, 
            c(diskmap$spaces,0)), function(i,b,s) {
              c(rep(i,b),rep(".",s)) %>% str_c(collapse = "")
            }) %>% 
    str_c(collapse = "")
}

compress = function(diskmap) {
  
  nspace = diskmap$spaces %>% sum()
  disk = print_diskmap(diskmap) %>% str_split("", simplify = T)
  
  filling = rev(disk[disk != "."])[1:nspace]
  disk[disk == "."] <- filling
  str_c(disk, collapse = "") %>% str_sub(1, -nspace - 1)
}

compressed_disk = compress(diskmap) %>%
  str_split("", simplify = T) %>% as.integer()

imap_dbl(compressed_disk, function(v,i) {(i - 1)*v}) %>% 
  sum()

# 88217448737 too low

# Part 2 ####
