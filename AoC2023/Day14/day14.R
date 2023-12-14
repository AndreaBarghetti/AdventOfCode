library(tidyverse)
library(AoCUtils)

input <- read_lines("AoC2023/Day14/input.txt")

panel = str_split(input,"", simplify = T)

# part 1 ####
score_matrix = matrix(rev(1:nrow(panel)), nrow = nrow(panel), ncol = ncol(panel))

panel = str_replace_all(panel, c("\\."="0","O"="1","#"="2")) %>% 
  as.integer() %>% matrix(nrow = nrow(panel))

tilt_line <- function(line, rev=F) {
  if(rev){line=rev(line)}
  idx = lag(cumsum(line==2), default = 0)
  lines = map(unique(idx), function(i){
    line[idx==i]
  }) %>% map(sort)
  line = unlist(lines)
  if(rev){line=rev(line)}
  line
}

tilt_panel <- function(panel, dir="N") {
  if(dir=="N"){
    panel = apply(panel,2, function(line){
      tilt_line(line, rev = T)
    })
  }
  if(dir=="S"){
    panel = apply(panel,2, function(line){
      tilt_line(line, rev = F)
    })
  }
  if(dir=="W"){
    panel = apply(panel,1, function(line){
      tilt_line(line, rev = T)
    }) %>% t()
  }
  if(dir=="E"){
    panel = apply(panel,1, function(line){
      tilt_line(line, rev = F)
    })%>% t()
  }
  panel
}

tilted = tilt_panel(panel,"N")

sum(score_matrix[tilted==1])

# Part 2 ####
tilt_cycle = function(panel) {
  for (dir in c("N","W","S","E")) {
    panel = tilt_panel(panel,dir = dir)
  } 
  panel
}
tilt_cycles <- function(panel, cycles) {
  for (i in 1:cycles) {
    panel = tilt_cycle(panel)
  }
  panel
}

get_loop = function(panel) {
  
  cache = rlang::env()
  i=0
  repeat {
    i=i+1
    key <- toString(panel) %>% str_remove_all(" |,|2")
    
    if (exists(key, envir = cache)) {
      return(c(i, get(key, envir = cache)))
    }
    
    cache[[key]] <- i
    
    panel = tilt_cycle(panel)
  }

}

loop = get_loop(panel)

remain_cycles = (1000000000-loop[2])%%(loop[1]-loop[2])

tilted = tilt_cycles(panel,remain_cycles+loop[2])

sum(score_matrix[tilted==1])


# vis ####
library(animation)

plot_panel <- function(panel) {
  panel %>% 
    mat_to_tibble() %>% 
    mutate(color=recode(value,"0"="black","1"="#ab813e","2"="#ababab")) %>% 
    ggplot(aes(x=x,y=y, fill=color)) +
    theme_void()+
    coord_equal() +
    geom_tile(show.legend = F) +
    scale_fill_identity()
}

save_animation <- function() {
  animation::saveGIF({
    dirs = circular(c("N","W","S","E"))
    for (i in 1:100) {
      print(plot_panel(panel))
      panel = tilt_panel(panel,dir = dirs[i])
    }
  },
  interval=.2,
  loop=T, 
  movie.name = "day14.gif",
  ani.width = 400,
  ani.height = 400,
  outdir = "AoC2023/GIFs/",
  clean = T)
}
save_animation()

move_rocks <- function(line, rev=T) {
  if(!rev){line=rev(line)}
  for (r in which(line==1)) {
    if(r==1){next}
    if(line[r-1]==0){line[r-1]<-1;line[r]<-0}
  }
  if(!rev){line=rev(line)}
  line
}

tilt_panel_step <- function(panel, dir="N") {
  if(dir=="N"){
    panel = apply(panel,2, function(line){
      move_rocks(line, rev = T)
    })
  }
  if(dir=="S"){
    panel = apply(panel,2, function(line){
      move_rocks(line, rev = F)
    })
  }
  if(dir=="W"){
    panel = apply(panel,1, function(line){
      move_rocks(line, rev = T)
    }) %>% t()
  }
  if(dir=="E"){
    panel = apply(panel,1, function(line){
      move_rocks(line, rev = F)
    })%>% t()
  }
  panel
}

save_animation2 <- function() {
  animation::saveGIF({
    dirs = circular(c("N","W","S","E"))
    for (i in 1:12){
      for (s in 1:20) {
        print(plot_panel(panel))
        panel = tilt_panel_step(panel,dir = dirs[i])
      }
    }
  },
  interval=.05,
  loop=T, 
  movie.name = "day14.gif",
  ani.width = 400,
  ani.height = 400,
  outdir = "AoC2023/GIFs/",
  clean = T)
}
save_animation2()


