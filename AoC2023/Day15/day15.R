library(tidyverse)
input <- read_lines("AoC2023/Day15/input.txt")

steps = str_split(input, ",", simplify = T)

# Part 1 ####
hash <- function(label) {
  label = str_split(label,"",simplify = T)
  nums = map_int(label, utf8ToInt)
  value = 0
  for (x in nums) {
    value=((value+x)*17)%%256
  }
  value
}
sum(map_int(steps,hash))

# Part 2 ####
do_step = function(step,boxes) {
  type = str_extract(step,"[-=]")
  label = str_extract(step, "^\\w+")
  box = hash(label)+1
  value = str_extract(step,"\\d+") %>% as.integer()
  
  if (type=="-") {
    if(!is.null(boxes[[box]][label])) {
      boxes[[box]] <- boxes[[box]][names(boxes[[box]])!=label]
    }
  }
  if (type=="=") {
    if(is.null(boxes[[box]][label])) {
      boxes[[box]] <- c(boxes[[box]], setNames(value,label))
    } else {
      boxes[[box]][label] <- value
    }
  }
  boxes
}

do_steps <- function(steps) {
  boxes = vector(mode = "list",length = 256)
  for (step in steps) {
    boxes <- do_step(step, boxes)
  }
  boxes
}

get_power <- function(boxes){
  powers = imap_dbl(boxes, function(box,i){
    sum(seq_along(box)*box*i)
  })
  sum(powers)
}

get_power(do_steps(steps))

# vis ####
library(animation)

boxes=do_steps(steps)

plot_boxes <- function(boxes) {
  space = matrix(1:256, nrow = 16,ncol = 16, byrow = T)
  boxes_df = imap_dfr(boxes, function(box, i){
    rc= which(space==i,arr.ind = T)
    tibble(r = rc[1],
           c = rc[2],
           value = box,
           label = names(box)) %>% 
      mutate(order=desc(row_number()))
  }) 
  if(is.null(boxes_df$value)) {boxes_df$value<-0}
    boxes_df %>% 
      mutate(value = replace_na(value, 0)) %>%
      arrange(desc(order)) %>% 
      ggplot(aes(x=c,y=r,fill=value, size=order)) +
      geom_point(show.legend = F, shape=22)+
      theme_void() +
      scale_fill_viridis_c() +
      scale_size_continuous(range = c(6,10))
}

save_animation <- function(){
  animation::saveGIF({
    boxes = vector(mode = "list",length = 256)
    i=0
    for (step in steps) {
      i=i+1
      # print(i)
      boxes <- do_step(step, boxes)
      if((i%%10)==0){
        print(plot_boxes(boxes))
      }
    }
  },
  interval=.02,
  loop=T, 
  movie.name = "day15.gif",
  ani.width = 400,
  ani.height = 400,
  outdir = "AoC2023/GIFs/",
  clean = T)
}
save_animation()


