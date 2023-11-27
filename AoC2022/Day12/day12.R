library(tidyverse)

input <- read_lines("Day12/input.txt")
source('Day12/functions.R')

cmap <- str_split(input, "", simplify = T)

# part1 ####

dist_map <- get_dist_map(cmap)

res1 <- dist_map[cmap=="E"]
res1

# part2 ####
# invert the map
# and go from end to start
get_cmap_info <- function(cmap) {

  # invert start and end position
  start <- which(cmap == "E",arr.ind = FALSE)
  end <- which(cmap == "S",arr.ind = FALSE)
  # reverse height
  map <- reset_map(cmap)*(-1)
  posmap <- make_posmap(cmap)
  list(map=map,
       posmap=posmap,
       start=start,
       end=end)
}

dist_map2 <- get_dist_map(cmap)
res2 <- dist_map2[cmap=="a" & dist_map2!=0] %>% min()
res2

# Visualization ####
source('Day12/functions.R')

old_start <- which(cmap=="S")
new_start <- which(dist_map2==res2 & cmap=="a")

info <- get_cmap_info(cmap)

# take all possible paths until reach end
path <- list()
start <- info$start
pos <- start
current_step <- dist_map[start]


# path_df <- map_dfr(path, function(pos) {
#   which(posmap==pos, arr.ind = T) %>% as_tibble()
# })

reshape2::melt(t(dist_map2)) %>%
  ggplot(aes(x=Var1, y=-Var2, fill=(value))) +
  geom_tile(show.legend = F, ) +
  theme_void() +
  coord_equal() +
  scale_fill_viridis_c()
  geom_path(inherit.aes = F, aes(x=col, y=-row) , show.legend = F, col="red", size=.5, alpha=.5, data={
    path_df
  })
