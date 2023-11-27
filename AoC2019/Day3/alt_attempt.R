library(tidyverse)
library(AoCUtils)
source("Day3/segments.R")

input <- read_aoc_input_with(year = 2019, day = 3, .fun = read_lines)

read_wire <- function(x) {
  txt <- x %>% 
    str_split(",", simplify = T)
  dirs = str_sub(txt,1,1)
  values = str_extract(txt, '\\d+') %>% as.integer()
  list(dirs=dirs,values=values)
}

wires <- map(input, read_wire)

# Part 1 ####
new_point <- function(p, d, v) {
  
  if (d == "U") {
    x <- p[1]
    y <- p[2]+v
  }
  if (d == "D") {
    x <- p[1]
    y <- p[2]-v
  }
  if (d == "L") {
    y <- p[2]
    x <- p[1]-v
  }
  if (d == "R") {
    y <- p[2]
    x <- p[1]+v
  }
  c(x,y)
}

extend_wire <- function(wire) {
  space <- matrix(0, nrow=length(wire$dirs)+1, ncol=2)
  current_p <- c(0,0)
  walk(seq_along(wire$dirs), function(i) {
    current_p <<- new_point(current_p, wire$dirs[i], wire$values[i])
    space[i+1,] <<- current_p
  })
  space
}

map_wire <- function(exd_wire) {
  
  exd_wire %>% 
    cbind(lead(exd_wire)) %>% 
    `colnames<-`(c("x0","y0","x","y")) %>% 
    `[`(.,-nrow(.),)
  
}

wire_maps <- map(wires, extend_wire) %>% 
  map(map_wire) %>% 
  map(as_tibble)

wire_maps <- wire_maps %>% 
  map(~{.x %>% 
      mutate(seg = pmap(., function(x0,y0,x,y) {segment(c(x,x0,y,y0))}))
    
  })

wire_segs <- map(wire_maps, ~{
  apply(.x[,c(1,3,2,4)], 1, segment, simplify = F)
})

all_intersections <- map(wire_maps[[1]]$seg,function(s1) {
  map(wire_maps[[2]]$seg, function(s2) {
    intersect(s1,s2)
  }) %>% purrr::reduce(rbind)
}) %>% purrr::reduce(rbind)

dist = all_intersections %>% 
  unique() %>% 
  apply(1, function(x) {sum(abs(x))})

all_intersections <- cbind(all_intersections, dist)

all_intersections[order(dist),] %>% 
  head(1)

# Part 2 ####
wire_maps <- map(wire_maps, function(wp){
  wp %>% 
    mutate(w_dist = cumsum(map_dbl(seg, ~.x$length)))
})


# vis
p <- wire_maps %>%
  purrr::reduce(bind_rows, .id="wire") %>% 
  ggplot(aes(x=x0, y=y0, xend=x,yend=y, label=w_dist)) +
  geom_segment(aes(col=wire), show.legend = F) +
  theme_void() +
  geom_point(data=NULL, x=0,y=0, col="black") +
  geom_point(data=as_tibble(all_intersections), inherit.aes = F, aes(x=x.x0,y=y.y1), col="black", shape=21, size=1) +
  coord_equal()

plotly::ggplotly(p)
