library(tidyverse)
input <- read_lines("AoC2023/Day17/input.txt")

map <- str_split(input,"", simplify = T) %>% 
  apply(2, as.integer)

# Part 1 ####
# this takes a while to run
run_crucibles <- function(map) {
  
  state = c(posr=1,posc=1,dirr=0,dirc=1,speed=0,heat_lost=0)
  
  end=c(posr=nrow(map), posc=ncol(map))
  
  cache=rlang::env()
  
  queue = collections::priority_queue()
  queue$push(state, -state['heat_lost'])
  
  while (queue$size()>0) {
    
    state = queue$pop()
    
    pos = c(state["posr"],state["posc"])
    dir = c(state["dirr"],state["dirc"])
    speed = state["speed"]
    heat_lost = state["heat_lost"]
    
    if (isTRUE(all.equal(pos, end))) {
      return(heat_lost)
    }
    
    if(dir[1]==0) {
      L=c(dirr=1,dirc=0);R=c(dirr=-1,dirc=0)
    } else {
      L=c(dirr=0,dirc=1);R=c(dirr=0,dirc=-1)
    }
    
    pos_fw = pos+dir
    pos_L = pos+L
    pos_R = pos+R
    
    state_fw = c(pos_fw,dir,speed+1,heat_lost)
    
    state_L = c(pos_L,L,speed=1,heat_lost)
    
    state_R = c(pos_R,R,speed=1,heat_lost)
    
    if(speed<3) {
      next_states <- list(state_fw,state_L, state_R)
    } else {
      next_states <- list(state_L, state_R)
    }
    
    for (state in next_states) {
      #if out of map
      if (any(state[1:2]<=0) | state[1] >nrow(map) | state[2]>ncol(map)){
        next
      }
      state['heat_lost'] <- state['heat_lost']+map[state["posr"],state["posc"]]
      
      label = toString(state[1:5])
      
      # if cached
      if (!is.null(cache[[label]])) {
        if (state['heat_lost'] >= cache[[label]]) {
        next
        }
      } 
      cache[[label]] <- state['heat_lost']
      
      queue$push(state, -state['heat_lost'])
    }
    
  }
  
}

result = run_crucibles(map)

# part 2 #### 

# works but takes a while
run_ultracrucibles <- function(map) {
  
  state = c(posr=1,posc=1,dirr=0,dirc=1,speed=0,heat_lost=0)
  
  end=c(posr=nrow(map), posc=ncol(map))
  
  cache=rlang::env()
  
  queue = collections::priority_queue()
  queue$push(state, priority = -state['heat_lost'])
  
  while (queue$size()>0) {
    
    state = queue$pop()

    pos = c(state["posr"],state["posc"])
    dir = c(state["dirr"],state["dirc"])
    speed = state["speed"]
    heat_lost = state["heat_lost"]
    
    # if end
    if (isTRUE(all.equal(pos, end))) {
      if (state['speed']>=4){
        return(heat_lost) 
      } #else {next}
    }
    
    if(dir[1]==0) {
      L=c(dirr=1,dirc=0);R=c(dirr=-1,dirc=0)
    } else {
      L=c(dirr=0,dirc=1);R=c(dirr=0,dirc=-1)
    }
    
    pos_fw = pos+dir
    pos_L = pos+L*4
    pos_R = pos+R*4
    
    state_fw = c(pos_fw,dir,speed+1,heat_lost)
    
    state_L = c(pos_L,L,speed=4,heat_lost)
    
    state_R = c(pos_R,R,speed=4,heat_lost)
    
    if(speed==0) {
      next_states <- list(state_fw, state_L, state_R)
    } else if (speed<4) {
      next_states <- list(state_fw)
    } else if (speed<10) {
      next_states <- list(state_fw, state_L, state_R)
    } else {
      next_states <- list(state_L, state_R)
    }
    
    for (state in next_states) {
      #if out of map
      if (any(state[1:2]<=0) | state[1] >nrow(map) | state[2]>ncol(map)){
        next
      }
      
      state['heat_lost'] <- state['heat_lost']+sum(map[pos[1]:state["posr"],pos[2]:state["posc"]])-map[pos[1],pos[2]]
      # state['heat_lost'] <- state['heat_lost']+map[state["posr"],state["posc"]]
      
      label = toString(state[1:5])
      
      # if cached
      if (!is.null(cache[[label]])) {
        if (state['heat_lost'] >= cache[[label]]) {
          next
        }
      } 
      cache[[label]] <- state['heat_lost']
      
      queue$push(state, -state['heat_lost'])
      
    }
    
  }
  
}

result2 = run_ultracrucibles(map)

