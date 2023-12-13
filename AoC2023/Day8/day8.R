library(tidyverse)
library(AoCUtils)
library(gmp)

input <- read_lines("AoC2023/Day8/input.txt")

directions <- input[[1]] %>% str_split("") %>% unlist()

parse_input = function(input) {
  
  input = input[-c(1:2)]
  sites = str_extract(input, "^\\w+")
  L = str_extract(input, "(?<=\\()\\w+")
  R = str_extract(input, "\\w+(?=\\))")
  
  map2(L,R, function(L,R) {list(L=L,R=R)}) %>% 
    setNames(sites)
}

map <- parse_input(input)

# Part 1 ####
follow_directions <- function(map, directions, start="AAA", end="ZZZ") {
  directions <- circular(directions)
  pos = start
  i=0
  while(pos != end) {
    i=i+1
    pos = map[[pos]][[directions[i]]]
  }
  return(i)
}

follow_directions(map, directions)

# Part 2 ####
get_loop_len = function(map, directions, start) {
  directions <- circular(directions)
  pos = start
  l=length(directions)
  i=1

  states = list()
  t = c()
  
  repeat {
    pos = map[[pos]][[directions[i]]]
    i=i+1
    if(str_sub(pos,-1)=="Z") {
      t=c(t,i)
      state = str_c(pos,i%%l)
      if (!is.null(states[[state]])){break}
      states[[state]] <- c(states[[state]],i)
    }
  }
  t-1
}

loops = map(str_subset(names(map),"A$"), function(start) {
  t = get_loop_len(map, directions, start)
  t
})
names(loops) <- str_subset(names(map),"A$")

# LCM of two numbers
lcm_two <- function(a, b) {
  abs(a * b) / gcd(a, b)
}

# LCM of a vector of numbers
lcm_multiple <- function(numbers) {
  Reduce(lcm_two, numbers)
}

lcm_multiple(map_int(loops, diff)) %>% 
  format(scientific = F)


### vis
library(gganimate)

generate_circle_points <- function(n) {
  # Calculate the angles for each point
  angles <- seq(0, 2 * pi, length.out = n + 1)
  angles <- angles[-length(angles)]  # Remove the last point as it's the same as the first
  
  # Calculate x and y coordinates
  x <- cos(angles)
  y <- sin(angles)
  
  # Create a dataframe of points
  points <- data.frame(x = x, y = y)
  return(points)
}

places = tibble(place=names(map),L=map_chr(map,~.x$L), R=map_chr(map,~.x$R)) %>% 
  mutate(pos = generate_circle_points(n())) %>% 
  unnest(pos) 

places <- places %>% 
  left_join(select(places,place, lx=x,ly=y),by = c("L"="place")) %>% 
  left_join(select(places,place, rx=x,ry=y),by = c("R"="place"))

directions <- circular(directions)
pos = str_subset(names(map),"A$")

df = map_dfr(1:30, function(i){
  pos <<- map_chr(pos, function(p) {
    map[[p]][[directions[i]]]
  })
  df = tibble(place=pos, group=seq_along(pos))
  
  places %>% 
    inner_join(df,by = join_by(place)) %>% 
    mutate(time=i) 
})

animation = df %>% 
  ggplot(aes(x=x,y=y,color = place)) +
  geom_point(size=5, show.legend = F, aes(group = group)) +
  theme_void() +
  gganimate::transition_states(states = time, 
                               transition_length = 20, state_length = 1) +
  gganimate::ease_aes('linear') +
  enter_fade()+
  exit_fade() +
  geom_segment(data=places, aes(xend=lx,yend=ly), col="black", linewidth=.1) +
  geom_segment(data=places, aes(xend=rx,yend=ry), col="black", linewidth=.1) +
  scale_color_viridis_d()

gganimate::anim_save(
  gganimate::animate(animation, rewind = T,nframes = 300, duration = 30),
  file = "AoC2023/GIFs/day8.gif")
