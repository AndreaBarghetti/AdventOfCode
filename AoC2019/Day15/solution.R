library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 15, .fun = read_lines)
program <- input %>% str_split(",", simplify = T) %>% as.numeric()

# part 1 ####
infinite <- function(x, default) {
  obj <- x
  class(obj) <- c("infinite", class(obj))
  attr(obj, "default") <- default
  return(obj)
}

`[.infinite` <- function(x, i) {
  
  if (i > length(x)) {
    res <- attr(x, "default")
  } else { res <- NextMethod("[") }
  if (i <= 0) {
    return(attr(x, "default"))}
  if (is.na(res)) {
    return(attr(x, "default"))}
  if (is_empty(res)) {
    return(attr(x, "default"))}
  return(res)
}

Computer <- R6::R6Class("Computer",
                        
                        public = list(
                          i = NA,
                          status = NA,
                          program = NA,
                          rel_base = NA,
                          outputs = NA,
                          
                          initialize = function(program) {
                            self$program <- zeroindex(infinite(as.numeric(program),0))
                            self$i <- 0
                            self$status <- "running"
                            self$rel_base <- 0
                            self$outputs <- list()
                          },
                          
                          
                          read_instruction = function(instr) {
                            (instr +100000) %>% str_sub(c(5,4,3,2), c(6,4,3,2)) %>% 
                              as.integer() %>% setNames(c("op", "m1","m2","m3"))
                          },
                          
                          get_param = function(mode, n) {
                            param <- self$program[self$i+n]
                            if (mode == 2) {
                              param <- param + self$rel_base
                            }
                            param
                          },
                          
                          get_value = function(mode,param) {
                            
                            if (mode==0) {
                              value <- self$program[param]
                            } else if (mode==1) {
                              value <- param
                            } else if(mode==2) {
                              value <- self$program[param]
                            } else {stop("wrong mode")}
                            
                            return(value)
                          },
                          
                          step = function(input=NULL) {
                            
                            instruction <- self$read_instruction(self$program[self$i])
                            
                            op <- instruction['op']
                            
                            p1 <- self$get_param(instruction['m1'],1)
                            p2 <- self$get_param(instruction['m2'],2)
                            p3 <- self$get_param(instruction['m3'],3)
                            
                            v1 <- self$get_value(instruction['m1'], p1)
                            v2 <- self$get_value(instruction['m2'], p2)
                            v3 <- self$get_value(instruction['m3'], p3)
                            
                            # operations 
                            if (op == 1) {
                              self$program[p3] <- v1+v2
                              self$i <- self$i + 4
                            } else if (op == 2) {
                              self$program[p3] <- v1*v2
                              self$i <- self$i + 4
                            } else if (op == 3) {
                              if(is.null(input) | is_empty(input)) {
                                self$status <- "waiting"
                                message("input required")
                              } else {
                                self$status <- "running"
                                self$program[p1] <- input
                                input <- input[-1]
                                self$i <- self$i + 2
                              }
                              
                            } else if (op == 4) {
                              self$i <- self$i + 2
                              self$outputs <- c(self$outputs, as.numeric(v1))
                              message(c("#",".","X")[v1+1])
                              return(as.numeric(v1))
                            } else if (op == 5) {
                              if (v1 != 0) {self$i <- v2} else {self$i <- self$i + 3}
                            } else if (op == 6) {
                              if (v1 == 0) {self$i <- v2} else {self$i <- self$i + 3}
                            } else if (op == 7) {
                              if (v1 < v2) {self$program[p3] <- 1} else {self$program[p3] <- 0}
                              self$i <- self$i + 4
                            } else if (op == 8) {
                              if (v1 == v2) {self$program[p3] <- 1} else {self$program[p3] <- 0}
                              self$i <- self$i + 4
                            } else if (op == 9) {
                              self$rel_base <- self$rel_base + v1
                              self$i <- self$i + 2
                            } else if (op == c(99)) {
                              self$status <- "finished"
                              message("program run succesfully")
                              return(0)
                            } else {
                              stop(paste(self$program[self$i], "Error: ",op,"is not a valid op" ))
                            }
                          },
                          
                          run = function(input=NULL) {
                            self$status <- "running"
                            while (self$status == "running") {
                              self$step(input)
                              input=input[-1]
                            }
                          },
                          
                          print = function() {
                            cat("i:", self$i, "; ",
                                "status:", self$status, "\n")
                          }
                          
                        )
)

map_to_tibble <- function(map) {
  map %>% 
    imap_dfr(function(y,x){
      x=as.integer(x)
      value=as.character(y) 
      y=as.integer(names(y))
      return(tibble(x=x,y=y,value=value))
    })
}

plot_map <- function(map) {
  df= map_to_tibble(map)
  ggplot(df, aes(x=x,y=y, fill=value)) +
    geom_tile() +
    coord_equal() +
    theme_bw() 
}

convert_dir <- function(dir) {
  if (dir=="U") {pos = c(0,1)}
  if (dir=="D") {pos = c(0,-1)}
  if (dir=="L") {pos = c(-1,0)}
  if (dir=="R") {pos = c(1,0)}
  pos
}

Robot <- R6::R6Class("Robot",
                     public = list(
                       
                       computer = NA,
                       
                       map = list('0'=list('0'=".")),
                       moves = c("U"=1,"D"=2,"L"=3,"R"=4),
                       signals = c("#"=0,"."=1,"X"=2),
                       steps=0,
                       OS_dist = NA,
                       position = c(0,0),
                       
                       initialize = function(program) {
                         self$computer = Computer$new(program)
                       },
                       
                       move = function(dir=names(self$moves)) {
                         dir = match.arg(dir)
                         pos = convert_dir(dir)+self$position
                         dir = recode(dir, !!!self$moves)
                         
                         self$computer$run(dir)
                         out = c("#",".","X")[rev(self$computer$outputs)[[1]]+1]
                         
                         #annonate map and change position
                         self$update_map(out,pos)
                         
                         return(out)
                       },
                       
                       update_map = function(output,pos) {
                         cpos = as.character(pos)
                         isnew=is.null(self$map[[cpos[1]]][[cpos[2]]])
                         self$map[[cpos[1]]][[cpos[2]]]<-output
                         
                         if (output %in% c(".","X")) {
                           self$position = pos
                           
                           self$steps <- self$steps+ifelse(isnew,1,-1)
                           if (output=='X'){self$OS_dist <- self$steps}
                           
                         }
                       },
                       
                       diplay = function() {
                         plot_map(self$map)+
                           annotate(geom = "point", 
                                    x=self$position[1],
                                    y=self$position[2])
                       },
                       
                       print = function() {
                         
                       }
                       
                     )
)


robot <- Robot$new(program)


#left hand method
explore_all <- function(robot, dir="U") {
  dirs =c("U","R","D","L")
  repeat {
    out = robot$move(dir)
    if (out !="#") {
      dir=recode(dir, "U"="L","L"="D","D"="R","R"="U")
      if(isTRUE(all.equal(robot$position, c(0,0)))) {break}
      }
    if (out =="#") {dir=recode(dir, "U"="R","R"="D","D"="L","L"="U")}
  }
}

suppressMessages(explore_all(robot, "U"))

robot$diplay() +
  annotate(geom = "tile",x = 0,y=0,fill='yellow') +
  scale_fill_manual(values=c('gray','black','red')) +
  annotate(geom = "point", 
           col="green",
           size=2,
           x=robot$position[1],
           y=robot$position[2])

robot$OS_dist

# part2 ####

# this is a very bad and slow method... 
# (should do a proper BFS instead)
spread_oxygen <- function(robot) {
  
  map <- robot$map %>% 
    map_to_tibble() %>% 
    filter(value!="#") %>% 
    mutate(oxygen = ifelse(value=='X',1,0)) %>% 
    mutate(current=ifelse(value=='X',T,F))
  
  time = 0
  
  while (any(map$oxygen==0)) {
    time <- time+1

    new = map %>% 
      filter(oxygen==1,current) %>% 
      transmute(data=pmap(., function(x,y,...){
        spreadx=tibble(x=c(x-1,x+1),y=y, oxygen=1)
        spready=tibble(x=x,y=c(y-1,y+1), oxygen=1)
        bind_rows(spreadx,spready)
      })) %>% 
      unnest(data) %>% 
      distinct() %>% 
      semi_join(map, by = join_by(x, y)) %>% 
      mutate(time = time,current=T)
    
    map <- map %>% 
      anti_join(new, by = join_by(x, y)) %>% 
      mutate(current=F) %>% 
      bind_rows(new) 
  }
  
  map
  
}

ox_map <- spread_oxygen(robot)
max(ox_map$time)

