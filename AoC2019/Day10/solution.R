library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 10, .fun = read_lines)

map <- input %>% str_split("", simplify = T)


# part 1 ####

get_coordinates <- function(map) {
  coords <- c()
  for (x in 1:ncol(map)) {
    for (y in 1:nrow(map)) {
      if (map[y,x]=="#") {
        coords <- rbind(coords, c(x=x,y=y))
      }
    }
  }
  coords
} 
coords <- get_coordinates(map)

n_asteroids = sum(map=="#")

# all possible segments:
all_segs <- combn(n_asteroids, m = 2) %>% 
  apply(2, function(i) {
    segment(c(coords[i[1],],coords[i[2],]))
  }, simplify = F)


# is an segment cut by any asteroid
is_segment_cut <- function(segment, coords) {
  
  for (i in 1:nrow(coords)) {
    if (seg_include(segment, 
                    coords[i,1], 
                    coords[i,2],
                    esclude_extremes=TRUE)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

cut_segments <- map_lgl(all_segs, function(seg) {
  is_segment_cut(seg, coords)
}) 

# save(cut_segments, file = "Day10/cut_segments")

direct_segs <- all_segs[!cut_segments]

count_visibles <- map_dfr(direct_segs, ~{
  tibble(x=.x[c(1,3)], y=.x[c(2,4)])
}) %>% 
  group_by(x,y) %>% 
  count() %>% 
  ungroup()

count_visibles %>% 
  arrange(desc(n))


# part 2 ####
ast_base <- count_visibles %>% 
  filter(n==max(n))

get_angle <- function(x,y){
  angle = atan2(x,y)*180/pi 
  angle = ifelse(angle<0, angle+360, angle)
  angle
}

ast_sorted <- coords %>% 
  as_tibble() %>% 
  mutate(rx=x-ast_base$x,
         ry=ast_base$y - y, 
         angle = get_angle(rx,ry),
         dist=sqrt(rx^2+ry^2)) %>% 
  filter(!(rx==0&ry==0)) %>% 
  arrange(angle, dist) %>% 
  group_by(angle) %>% 
  mutate(round=row_number()) %>% 
  ungroup() %>% 
  arrange(round,angle) %>% 
  mutate(order = row_number())

ast_sorted %>% 
  filter(order==200) %>% 
  mutate(result = (x-1)*100 + (y-1)) %>% 
  pull(result)


# vis ####
count_visibles %>% 
  mutate(y=max(y)-y+1) %>% 
  ggplot2::ggplot(ggplot2::aes(x = x, y = y)) + 
  ggplot2::geom_tile(ggplot2::aes(fill = n),show.legend = F) +
  ggplot2::coord_equal() +
  theme_void() +
  scale_fill_viridis_c()

ast_sorted %>% 
  mutate(y=max(y)-y+1) %>% 
  ggplot2::ggplot(ggplot2::aes(x = x, y = y)) + 
  ggplot2::geom_tile(ggplot2::aes(fill = order),show.legend = F) +
  ggplot2::coord_equal() +
  theme_void() +
  scale_fill_viridis_c(direction = -1)
