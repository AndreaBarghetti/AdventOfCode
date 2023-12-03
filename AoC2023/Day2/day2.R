library(tidyverse)
input <- read_lines("AoC2023/Day2/input.txt")

parse_input <- function(input) {
  
  map_dfr(input, function(inp) {
    splt = str_split(inp, ";|:", simplify = T)
    id = str_extract(splt[1], "\\d+") %>% as.integer()

    imap_dfr(splt[-1], function(x,i) {
      colors = x %>% 
        str_extract_all("Game|blue|green|red") %>% unlist() 
      values = x %>% 
        str_extract_all("\\d+")%>% unlist()  %>% 
        as.integer()
      tibble(color=colors, value=values, game=id, draw=i)
    })
    
    
  })
  
}
games = parse_input(input)

# Part 1 ####
games %>% 
  mutate(wrong = (color=="red"&value>12) | (color=="green"&value>13) | (color=="blue"&value>14)) %>%
  group_by(game) %>% 
  filter(all(!wrong)) %>% 
  pull(game) %>% unique() %>% sum()
 
# Part 2 ####
games %>% 
  group_by(game, color) %>%
  summarise(value=max(value), .groups = "drop") %>% 
  group_by(game) %>% 
  summarise(power=prod(value), .groups = "drop") %>% 
  pull(power) %>% sum()


# vis ####
rdn_cubes = function(n) {
  tibble(color=sample(c("red","blue","green"), size = n, replace = T),
         x= (runif(n,0,1)),
         y= (runif(n,0,1)),
  ) %>% 
    ggplot(aes(x=x,y=y,fill=color)) +
    geom_point(col="black", size=10, shape=sample(22:23,size = n, replace = T)) +
    scale_fill_identity() +
    theme_void() +
    theme(panel.border = element_rect(color = "black", fill="transparent", linewidth = 2)) +
    coord_fixed(xlim = 0:1, ylim = 0:1)
} 


library(gganimate)

df = map_dfr(1:20, function(frame) {
  n = floor(runif(1,1,30))
  tibble(color=sample(c("red","blue","green"), size = n, replace = T),
         x= (runif(n,0,1)),
         y= (runif(n,0,1)),
         frame=frame
  )
}) 

animation = df %>% 
  ggplot(aes(x=x,y=y,fill=color)) +
  geom_point(col="black", size=15, shape=sample(22:23,size = nrow(df), replace = T)) +
  scale_fill_identity() +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill="transparent", linewidth = 2)) +
  coord_fixed(xlim = 0:1, ylim = 0:1) +
  transition_states(states = frame, transition_length = .2) +
  exit_fade() +
  ease_aes('linear')

anim_save(animation, filename = "day2.gif", path = "GIFs", fps = 5)

