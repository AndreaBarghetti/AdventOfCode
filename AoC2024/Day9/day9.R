library(tidyverse)
input <- read_lines("AoC2024/Day9/input.txt")

parse = function(input) {
  
  t = str_split(input, "", simplify = T)
  size = t[seq(from = 1, to = length(t), by = 2)] %>% as.integer()
  spaces = t[seq(from = 2, to = length(t), by = 2)] %>% as.integer()
  
  list(id=seq_along(size)-1,
       size = size,
       spaces = c(spaces,0))
  
}

# Part 1 ####
diskmap = parse(input)

get_disk = function(diskmap) {
  pmap(list(diskmap$id,
            diskmap$size, 
            diskmap$spaces), function(i,b,s) {
              c(rep(i,b),rep(".",s))
            }) %>% unlist()
}

compress = function(diskmap) {
  
  nspace = diskmap$spaces %>% sum()
  disk = get_disk(diskmap)
  
  filling = rev(disk[disk != "."])[1:nspace]
  disk[disk == "."] <- filling
  disk[1:(length(disk)-nspace)] %>% as.numeric()
}

compressed_disk = compress(diskmap)

imap_dbl(compressed_disk, function(v,i) {(i - 1)*v}) %>% 
  sum() %>% format(scientific = F)


# Part 2 ####
better_diskmap = function(input) {
  
  sizes = input %>% 
    str_split("") %>% unlist() %>% 
    as.numeric()
  
  df = tibble(size = sizes) %>% 
    mutate(start=cumsum(lag(size,default = 1)),
           free = row_number()%%2==0)
  
  diskmap = df %>% 
    filter(!free) %>% 
    mutate(id = row_number()-1) %>% 
    select(id,start,size)
  
  return(diskmap)
}

diskmap = better_diskmap(input)

check_space = function(diskmap) {
  spaces = lead(diskmap$start)-(diskmap$start+diskmap$size)
  spaces[1:length(spaces)-1]
}

compress2 = function(diskmap) {
  for (id in rev(diskmap$id)) {
    i = which(id == diskmap$id)
    spaces=check_space(diskmap)
    fits = diskmap$size[i] <= spaces
    
    if (any(fits)) {
      goto = min(which(fits))
      if (goto<i) {
        diskmap[i,]$start <- diskmap[goto,]$start+diskmap[goto,]$size
        diskmap = arrange(diskmap, start)
      }
    }
  }
  diskmap
}

compressed_disk = compress2(diskmap)

get_disk = function(diskmap) {
  last = diskmap[nrow(diskmap),]
  v = rep(".",length = last$start + last$size-1)
  for(i in seq_along(diskmap$id)) {
    start = diskmap$start[i]
    size = diskmap$size[i]
    v[start:(start+size-1)] <- rep(diskmap$id[i],diskmap$size[i])
  }
  v
}

get_disk(compressed_disk) %>% 
  imap_dbl(function(v,i) {
    if(v=="."){return(0)}
    as.numeric(v)*(i-1)
  }) %>% sum() %>% format(scientific = F)
