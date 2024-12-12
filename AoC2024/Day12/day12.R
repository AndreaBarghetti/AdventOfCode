library(tidyverse)
input <- read_lines("AoC2024/Day12/input.txt")

map = str_split(input, "", simplify = T)

# Part 1 ####
get_plot_price = function(map, pos, discount=F) {

  moves = list(U=c(-1,0),
               D=c(1,0),
               L=c(0,-1),
               R=c(0,1))
  
  visited = matrix(F, nrow = nrow(map), ncol = ncol(map))
  
  plant = map[pos]
  queue = collections::queue()
  queue$push(pos)
  P=0
  A=0
  while (queue$size() >0) {
    p = queue$pop()
    
    # if out of bound
    if (any(p<=0)|any(p>dim(map))) { P=P+1 ; next }
    # if crossing fence
    if (map[p]!=plant) { P=P+1 ; next }
    # if already been there in same plot
    if (visited[p]) {next} else{visited[p] <- T}
    # otherwise walk to next
    if (map[p]==plant){
  
      A=A+1
      
      for (m in moves) {
        np = rbind(p+m)
        queue$push(np)
      }
      
      }
  }
  map[visited] = "." 
  
  # for pt2
  if (discount) {P = count_sides(visited)}
  
  return(list(name=plant, map=map, price=P*A))
}

get_field_price = function(map,discount=F) {
  
  price = 0
  
  while (any(map!=".")) {
    
    plant_pos = which(map!=".", arr.ind = T)
    pos = rbind(plant_pos[1,])

    result = get_plot_price(map, pos, discount)
    map = result$map
    price = price + result$price
  }
  
  return(price)
  
}

get_field_price(map,discount=F)

# Part 2 ####
count_sides = function(visited) {
  
  # expand
  extra_c <- matrix(F, nrow(visited), ncol = 1)
  visited <- cbind(extra_c, visited, extra_c)
  extra_r <- matrix(F, ncol(visited), nrow = 1)
  visited <- rbind(extra_r, visited, extra_r)
  
  count_sides_h = function(visited) {
    map_int( 1:(nrow(visited)-1), function(i) {
      l1 = visited[i,]
      l2 = visited[i+1,]
      v1 = l1&!l2
      v2 = l2&!l1
      sum(diff(v1) == 1,
          diff(v2) == 1)
    }) %>% sum()
  }
  
  h = count_sides_h(visited)
  v = count_sides_h(t(visited))
  
  h + v
  
}

get_field_price(map, discount=T)
