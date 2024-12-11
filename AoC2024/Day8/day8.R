library(tidyverse)
input <- read_lines("AoC2024/Day8/input.txt")

map = str_split(input, "", simplify = T)

antennas_ls = unique(as.vector(map[map!="."])) %>% 
  setNames(.,.)
antennas_ls = map(antennas_ls, ~which(map==.x, arr.ind = T))

# Part 1 ####

#get antinodes from 2 antennas
get_antinodes2 = function(A1, A2) {
  
  an1 = 2*A2-A1
  an2 = 2*A1-A2
  
  rbind(an1,an2)
}

#get antinodes from N antennas
get_antinodes = function(antennas) {
  
  row_combinations <- combn(nrow(antennas), 2)
  
  results <- apply(row_combinations, 2, function(idx) {
    A1 <- antennas[idx[1], ]
    A2 <- antennas[idx[2], ]
    get_antinodes2(A1, A2)
  }, simplify = F) %>% reduce(rbind) %>% 
    unique()

}

antinodes_ls = map(antennas_ls, get_antinodes)

antinodes = antinodes_ls %>% 
  reduce(rbind) %>% 
  unique()

# remove out of map
remove_oob = function(antinodes,map) {
  antinodes[antinodes[,1]>=1 & antinodes[,1]<= nrow(map) &
              antinodes[,2]>=1 & antinodes[,2]<= ncol(map),]
}

remove_oob(antinodes, map) %>% nrow()

# Part 2 ####
get_antinodes2 = function(A1, A2) {
 
  As = rbind(A1,A2) %>% as_tibble() 
  m = lm(row~col,As)

  cols = 1:ncol(map)
  rows = predict(m, newdata=tibble(col = cols))
  natnum = abs((rows - round(rows))) < 1e-12
  inmap = round(rows) > 0 & round(rows)<=nrow(map)
  
  cbind(round(rows)[natnum&inmap], round(cols[natnum&inmap]))
  
}

antinodes_ls = map(antennas_ls, get_antinodes)

antinodes = antinodes_ls %>% 
  reduce(rbind) %>% 
  unique()

nrow(antinodes)


##
print_map = function(print_antinodes=T) {
  
  amap = map
  if (print_antinodes) {
    amap[antinodes]<-"#"
    amap[map!="."]=map[map!="."]
  }
  apply(amap, 1, function(.x){
    cat(str_c(.x, collapse = ""))
    cat("\n")
  })
}

print_map()
