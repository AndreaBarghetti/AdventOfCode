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
