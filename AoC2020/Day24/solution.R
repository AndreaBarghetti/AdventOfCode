library(tidyverse)

# Day 24: Lobby Layout ####
input = read_lines('AoC2020/Day24/input.txt')
input = str_extract_all(input, 'e|ne|se|w|nw|sw')

# * - part 1 - *####
get_x = function(e,ne,se,w,nw,sw) {
  x = (e-w)+(ne+se-nw-sw)/2
  x
}
get_y = function(e,ne,se,w,nw,sw) {
  y = (nw+ne-sw-se)*sqrt(3)/2
  y
}

steps_df = map_dfr(input, ~{
  tibble(d = .x) %>% 
    count(d) %>%
    pivot_wider(names_from = d, values_from = n)
}) %>% 
  map_dfc(replace_na,0) 

tile_coords = steps_df %>% 
  transmute(x = get_x(e,ne,se,w,nw,sw),
            y = get_y(e,ne,se,w,nw,sw))
 
tile_coords %>% 
  count(x,y) %>%
  mutate(black = n%%2==1) %>% 
  count(black)
  
# * - part 2 - *####
# not very efficient, but it works

b_tiles = tile_coords %>% 
  count(x,y) %>%
  mutate(black = n%%2==1) %>% 
  filter(black) %>% 
  select(-n) %>% 
  mutate(x=round(x,digits = 3),
         y=round(y,digits = 3))


get_all_adj = function(x,y) {
  h=sqrt(3)/2
  tibble( x = x + c(-1,-1/2,+1/2,+1,+1/2,-1/2),
          y = y + c(0,h,h,0,-h,-h)) %>% 
    mutate(x=round(x,digits = 3),
           y=round(y,digits = 3))
}

get_w_tiles = function(b_tiles) {
  w_tiles = apply(b_tiles,1,function(tile) {
    get_all_adj(tile[1], tile[2])
  }) %>% do.call(rbind,.) %>% 
    unique() %>% 
    anti_join(b_tiles, by=c('x','y'))
  w_tiles
}

count_black_adj = function(x,y, b_tiles) {
  map2_int(x,y, function(x,y){
    sum( sqrt((x-b_tiles$x)^2+(y-b_tiles$y)^2) < 1.1 )
  })
}

next_b_tiles = function(b_tiles) {
  
  w_tiles = get_w_tiles(b_tiles)
  
  w_to_b = w_tiles %>% 
    filter(count_black_adj(x,y, b_tiles)==2)
  
  b_to_w = b_tiles %>% 
    # mutate(black = !((count_black_adj(x,y, b_tiles)-1)%in%c(1,2))) %>% 
    filter({
      adj= count_black_adj(x,y, b_tiles)-1
      between(adj, 1,2)
    })
  
  bind_rows(w_to_b, b_to_w)
    
  
}

game_of_tiles = function(b_tiles, rounds =100){
  
  for (i in 1:rounds){
    if (i%%10==0) {
      message(i,": ",nrow(b_tiles))
    }
    b_tiles = next_b_tiles(b_tiles)
  }
  b_tiles
  
}

tiles100 = game_of_tiles(b_tiles, 100)
pt2 = nrow(tiles100)

# animation:
library(animation)

plot_tiles= function(tiles, xlim = c(-30,30), ylim=c(-30,30)) {
  
  tiles %>% 
    ggplot() +
    geom_regon(aes(x0 = x, 
                   y0 = y, 
                   sides = 6, 
                   r = 1/sqrt(3), 
                   angle = pi/6),
               fill = "black", 
               color = 'white',linewidth = .1) +
    coord_fixed(xlim = xlim, ylim = ylim) +
    theme_void()
}

save_animation <- function(b_tiles, xlim, ylim) {
  animation::saveGIF({
    for (i in 1:100) {
      b_tiles = next_b_tiles(b_tiles)
      print(plot_tiles(b_tiles, xlim = xlim, ylim = ylim))
    }
  },
  interval=.2,
  loop=T, 
  movie.name = "day24.gif",
  ani.width = 500,
  ani.height = 500,
  clean = T)
}

save_animation(b_tiles, xlim = range(tiles100$x), ylim = range(tiles100$y))
