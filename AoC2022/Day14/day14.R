library(tidyverse)

input <- read_lines("Day14/input.txt")

source("Day14/functions.R")

# part 1 ####
coordinates <- get_coordinates(input)

map <- create_map(coordinates)

fullmap <- fill_map(map)

sum(fullmap)-sum(map)

# part 2 ####
map <- create_map(coordinates) %>% expand_map()
fullmap <- fill_map(map)

sum(fullmap)-sum(map)


# visualization ####
library(animation)

plot_sand <- function(map, map0) {
  reshape2::melt(t(map+map0)) %>%
    ggplot(aes(x=Var1, y=-Var2, fill=as.factor(value))) +
    geom_tile(show.legend = F) +
    theme_void() +
    coord_equal() +
    geom_point(aes(x=attributes(map0)$center, y=-1), fill="#ababab", size=3, shape=25, show.legend = F) +
    scale_fill_manual(values=c("#000000", "#ab813e","#ababab"))
}

map0 <- create_map(coordinates)
map <- map0

saveGIF(expr = {
  for(i in 1:625) {
    map <- add_sand(map)
    print(plot_sand(map, map0))
  }
},
loop=T,
interval=0.05,
ani.width = 312,
ani.height = 648,
movie.name = "animation.gif",
outdir = "Day14/",
clean = T
)





