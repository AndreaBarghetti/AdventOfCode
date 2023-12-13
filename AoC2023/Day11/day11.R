library(tidyverse)
input <- read_lines("AoC2023/Day11/input.txt")

space = str_split(input, "", simplify = T)

# Part 1 ####
get_stars_coords <- function(space, expansion=1e6) {
  
  empty_cols = apply(space, 2, function(c){
    all(c==".")
  }) %>% which()
  
  empty_rows = apply(space, 1, function(c){
    all(c==".")
  }) %>% which()
  
  stars = which(space=="#", arr.ind = T)
  
  stars[,2] <- map_int(stars[,2], ~{
    add = sum(empty_cols < .x) * (expansion-1)
    .x+add
  })
  stars[,1] <- map_int(stars[,1], ~{
    add = sum(empty_rows < .x) * (expansion-1)
    .x+add
  })
  stars
}

stars <- get_stars_coords(space, expansion = 2)

dist(stars, method = "manhattan", diag = T, upper = F) %>% 
  as.integer() %>% sum()

# Part 2 ####
stars <- get_stars_coords(space, expansion = 1e6)

dist(stars, method = "manhattan", diag = T, upper = F) %>% 
  as.integer() %>% sum()

# vis ####
library(gganimate)
stars <- get_stars_coords(space, expansion = 1)

stars_df = map_dfr(ceiling(seq(1,6,1)),function(exp) {
  exp=10^exp
  
  stars <- get_stars_coords(space, expansion = exp)
  as_tibble(stars) %>% 
    mutate(row=row-mean(row),
           col=col-mean(col),
           time=exp,
           group=row_number()) 
})

animation = stars_df %>%
  ggplot(aes(x=col,y=row)) +
  geom_point(shape=11,col='gold', aes(group=group)) +
  theme_void() +
  theme()
  gganimate::transition_states(states = time, transition_length = 10,state_length = 1)

gganimate::anim_save(
  gganimate::animate(animation, rewind = T, duration = 5, fps=10, bg='transparent'),
  file = "AoC2023/GIFs/day11.gif")
