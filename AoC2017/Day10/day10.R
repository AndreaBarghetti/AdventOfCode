library(tidyverse)
input <- read_lines('AoC2017/Day10/input.txt')

lengths = str_split(input, ',', simplify = T) %>% 
  as.integer()

# Part 1 ----
do_knot = function(seq, l, pos) {

  L = length(seq)
  part = (0:(l-1)+pos-1) %% L+1
  
  seq[part] = rev(seq[part])
  
  seq

}

do_knots = function(seq, lengths, c_pos=1, skip_size = 0) {

  L = length(seq)
  
  for (l in  lengths) {
    
    seq = do_knot(seq, l = l, pos = c_pos)
    
    c_pos = (c_pos + l + skip_size -1) %% L + 1
    skip_size = skip_size +1
  }
  
  list(skip_size = skip_size,
       seq=seq,
       pos = c_pos)
}

seq = do_knots(seq=0:255, lengths)$seq

prod(seq[1:2])

# Part 2 ----
operation = function(seq, input) {
  
  lengths = c(utf8ToInt(input),c(17, 31, 73, 47, 23))
  
  skip_size = 0
  pos = 1
  
  for (i in 1:64) {
    
    res = do_knots(seq, lengths, pos, skip_size)
    
    skip_size = res$skip_size
    pos = res$pos
    seq = res$seq
  }
  sparse_hash = seq
  
  dense_hash = matrix(sparse_hash, ncol = 16, byrow = T) %>% 
    apply(1,function(x) {
      purrr::reduce(x, bitwXor)
    })
  paste0(sprintf("%02X", dense_hash),collapse = "")
}

tolower(operation(0:255,input)) 
