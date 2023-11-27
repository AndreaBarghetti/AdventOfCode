library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 20, .fun = read_lines)

map <- str_split(input,"", simplify = T)

# map <- str_split(read_lines("Day20/test_input.txt"),"", simplify = T)

map <- mat_expand(map, n = 1, fill = " ")
# compress portal tiles into one position only
label_portals <- function(map) {
  
  portal_tiles <- which(matrix(map %in% LETTERS, nrow = nrow(map)), arr.ind = T)
  
  for (i in 1:nrow(portal_tiles)) {
    pos <- rbind(portal_tiles[i,])
    
    around <- map[rbind(pos,pos+c(1,0),pos+c(-1,0),pos+c(0,1),pos+c(0,-1))]
    
    if("." %in% around) {
      label <- map[rbind(pos,pos+c(1,0),pos+c(-1,0),pos+c(0,1),pos+c(0,-1))] %>% 
        str_subset("[A-Z]") %>% sort() %>% paste0(collapse = "") 
      map[pos] <- label
    } 
  }
  # remove remaining single letters
  map[str_detect(map, "^[A-Z]$")] <- " "
  return(map)
}

map <- label_portals(map)

display_map <- function(map) {
  plot_matrix(matrix(ifelse(str_detect(map, "[A-Z]"),"P",map),nrow(map))) +
    scale_fill_viridis_d()
}

start = which(map=="AA", arr.ind = T)
end = which(map=="ZZ", arr.ind = T)

# reuse from day 18
# not essential but quite a good trick
fill_dead_ends <- function(map) {
  
  moves = list(c(1,0),c(-1,0),c(0,1),c(0,-1))
  queue = list()
  
  # find first seeds
  for (r in 2:(nrow(map)-1)){
    for (c in 2:(ncol(map)-1)){
      pos <- cbind(r,c)
      value = map[pos]
      if (value=="." | (value %in% LETTERS)) {
        around_pos = map(moves, ~ pos+.x)
        around_vals = map_chr(around_pos, ~map[.x])
        if (sum(around_vals=="#")==3) {
          map[pos]<-"#"
          queue = c(queue, around_pos[around_vals!='#'])
        }
      }
    }
  }
  
  while (length(queue) > 0) {
    pos = queue[[1]]
    queue = queue[-1]
    
    value =  map[pos]
    if (value==".") {
      around_pos = map(moves, ~pos+.x)
      around_vals = map_chr(around_pos, ~map[.x])
      if (sum(around_vals=="#")==3) {
        map[pos]<-"#"
        queue = c(queue, around_pos[around_vals!='#'])
      }
    }
    
  }
  map
}

display_map(map)

map <- fill_dead_ends(map)

display_map(map)

# simple BFS from AA to ZZ
get_dist <- function(map, start, end) {
  
  moves <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1)) %>% 
    map(as.integer)
  
  # Create a queue and add the starting point
  queue <- list(start)
  visited <- matrix(FALSE, nrow(map), ncol(map))
  visited[start] <- TRUE
  visited[map=="#"] <- TRUE
  visited[map==" "] <- TRUE
  
  # Store the distances
  distances <- matrix(Inf, nrow(map), ncol(map))
  distances[start] <- -1
  
  # BFS loop
  
  while (length(queue) > 0) {

    current <- queue[[1]]
    queue <- queue[-1]
    
    # Check if the end is reached
    if (all(current== end)) {
      return(distances[end])
    }
    
    # Explore neighboring cells
    for (move in moves) {
      next_pos <- current + move
      
      # Check boundaries and walls
      if (any(next_pos<=0) | any(dim(map)-next_pos<0)) {next}
      
      if ((map[next_pos] == ".") && (!visited[next_pos])) {
        queue <- c(queue, list(next_pos))
        visited[next_pos] <- TRUE
        
        distances[next_pos] <- distances[current] + 1
        
      }
      # portals
      if (str_detect(map[next_pos], "[A-Z]{2}") && (!visited[next_pos])) {
        
        message("portal: ", map[next_pos])
        
        visited[next_pos] <- TRUE
        distances[next_pos] <- distances[current]
        
        if (all(next_pos == end)) {
          return(distances[end])
        }
        
        next_pos <- rbind(which(map == map[next_pos], arr.ind = T) %>% setdiff(next_pos))
        
        visited[next_pos] <- TRUE
        distances[next_pos] <- distances[current]
        
        queue <- c(queue, list(next_pos))
        
      }
    }
  }
  
  # Return Inf if no path is found
  return(Inf)
  
}
get_dist(map, start, end)

# part 2 ####

# update the map
# by hiding outer portals from first level (floor)
portals <- which(matrix(str_detect(map,"(?!AA|ZZ)[A-Z]{2}"), nrow(map)), arr.ind = T)
center <- c(nrow(map),ncol(map))%/%2

portals_df <- portals %>% 
  as_tibble() %>% 
  mutate(label = map[portals],
         dist = apply(portals,1, function(x) {dist(rbind(x,center))})) %>% 
  group_by(label) %>% 
  arrange(dist) %>% 
  mutate(type = ifelse(row_number()==1,"inner","outer")) %>% 
  ungroup() %>% 
  arrange(label)

outer_portals <- portals_df %>% 
  filter(type=="outer") %>% 
  column_to_rownames('label') %>% 
  select(row, col) %>% 
  as.matrix()

inner_portals <- portals_df %>% 
  filter(type=="inner") %>% 
  column_to_rownames('label') %>% 
  select(row, col) %>% 
  as.matrix()

map1 <- map # 1st floor
map2 <- map # all following next floors
map1[outer_portals] <- "#"
map2[map2 %in% c('AA','ZZ')]<-"#"

# stack 100 floors to start with.
# it's faster than having to add floors
map3d <- array(c(map1, rep(map2,100)), dim = c(dim(map1),101))

start = which(map3d=="AA", arr.ind = T)
end = which(map3d=="ZZ", arr.ind = T)

# in case more floors are needed
add_floor <- function(map3d, floor, n=1) {
  array(c(map3d,rep(floor,n)), dim = dim(map3d) + c(0,0,n)) 
}


# BFS from AA to ZZ, with the new rules
get_dist <- function(map3d, start, end) {
  
  moves <- list(c(-1, 0, 0), c(1, 0, 0), c(0, -1, 0), c(0, 1, 0)) %>% 
    map(as.integer)
  
  # Create a queue and add the starting point
  queue <- list(start)
  visited <- array(FALSE, dim = dim(map3d))
  visited_default_floor <- visited[,,2]
  visited[start] <- TRUE
  visited[map3d=="#"] <- TRUE
  visited[map3d==" "] <- TRUE
  
  # Store the distances
  distances <- array(Inf, dim = dim(map3d))
  distances_default_floor <- distances[,,2]
  
  distances[start] <- -1
  
  # BFS loop
  while (length(queue) > 0) {
    
    current <- queue[[1]]
    queue <- queue[-1]
    
    # Check if the end is reached
    if (all(current == end)) {
      return(distances[end])
    }
    
    # Explore neighboring cells

    for (move in moves) {
      
      next_pos <- current + move
      
      # Check boundaries and walls
      if (any(next_pos<=0) | any(dim(map3d)-next_pos<0)) {next}
      
      if ((map3d[next_pos] == ".") && (!visited[next_pos])) {
        queue <- c(queue, list(next_pos))
        visited[next_pos] <- TRUE
        
        distances[next_pos] <- distances[current] + 1
        
      }
      
      # portals
      if (str_detect(map3d[next_pos], "[A-Z]{2}") && (!visited[next_pos])) {
        
        portal = map3d[next_pos]
        # message("portal: ", portal)
        
        visited[next_pos] <- TRUE
        distances[next_pos] <- distances[current]
        
        if (all(next_pos == end)) {
          return(distances[end])
        }
        
        # if inner, floor up, if outer, floor down
        is_inner = all(next_pos[1:2] == inner_portals[portal,])
        is_outer = all(next_pos[1:2] == outer_portals[portal,])
        
        if (is_inner) {
          # add floors if needed (do 100 at a time)
          if (next_pos[3]+1 > dim(map3d)[3]) {
            map3d <- add_floor(map3d,map3d[,,2],100)
            visited <- add_floor(visited, visited_default_floor, 100)
            distances <- add_floor(distances, distances_default_floor, 100)
          }
          
          next_pos <- rbind(c(outer_portals[portal,], next_pos[3]+1))
        } else if (is_outer) {
          next_pos <- rbind(c(inner_portals[portal,], next_pos[3]-1))
        } else {stop("wtf...?")}

        visited[next_pos] <- TRUE
        distances[next_pos] <- distances[current]
        
        queue <- c(queue, list(next_pos))
        # explore lower floors first
        queue <- queue[order(map_int(queue, ~.x[3]))]
        
      }
    }
  }
  
  # Return Inf if no path is found
  return(Inf)
  
}

get_dist(map3d, start, end)
