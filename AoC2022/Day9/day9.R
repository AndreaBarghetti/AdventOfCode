library(tidyverse)

input <- readLines("Day9/input.txt")

source("Day9/functions.R")

# part 1  ####
tail_positions <- record_simulation(input, n_knots=2)
tail_positions %>%
  purrr::reduce(rbind) %>%
  unique() %>%
  nrow()

# part 2 ####
tail_positions <- record_simulation(input, n_knots=10)
tail_positions %>%
  purrr::reduce(rbind) %>%
  unique() %>%
  nrow()

# Visualization ####

#' save the whole rope in the env, instead of only the tail
#' and you'll have the positions of each knot at each move

move_r <- function(dir, rope, env) {

  n_knots <- length(rope)

  # move the head
  rope[[1]] <- move_k(dir = dir, knot = rope[[1]])

  #repeat for each knot
  for (k in 2:n_knots) {
    Head = rope[[k-1]]
    Tail = rope[[k]]
    Tail = move_t(Head, Tail)
    rope[[k]] <- Tail

    if (k ==n_knots) {
      rope_df <- rope %>%
        imap_dfr(function(x,n){
          as_tibble(t(x)) %>% mutate(knot=n)
        })
      append_tail_position(tail = list(Tail=rope_df), env = env)
    }
  }
  rope
}


n_knots=10
env <- rlang::env()
rlang::env_poke(nm = "tail_positions", value = list(), create = T, env = env)
rope <- make_rope(n_knots)

for (x in input) {
  move <- read_input(x)
  rope <- move_rope(move = move, rope = rope, env = env)
}

output <- rlang::env_get(env, nm = "tail_positions")

df_vis <- output[1:1000] %>%
  imap_dfr(function(x,i){
    mutate(x[[1]],i=i)
  })

plot_rope <- function(rope_df, xlim, ylim) {
  rope_df[[1]] %>%
    mutate(knot=1:10) %>%
    ggplot(aes(x=x,y=y)) +
    geom_path(size=4, col="white") +
    geom_point(aes(col=as.factor(knot)), size=4, show.legend = F) +
    theme_void() +
    coord_equal(xlim = xlim, ylim=ylim) +
    theme(panel.background = element_rect(fill = "black"))
}

saveGIF({
  for(i in 1:1000) {
    print(plot_rope(output[[i]],xlim = range(df_vis$x), ylim = range(df_vis$y)))
  }
},
loop=T,
interval=0.05,
ani.width = range(df_vis$x) %>% diff()*10,
ani.height = range(df_vis$y) %>% diff()*10,
movie.name = "animation.gif",
outdir = "Day9/",
clean = T
)

# other way
library(gganimate)

p <- df_vis %>%
  filter(i <=100) %>%
  # filter(knot %in% c("H","9")) %>%
  ggplot(aes(x=x,y=y)) +
  # geom_path(size=4, col="white") +
  geom_point(aes(col=knot), size=4, show.legend = F) +
  theme_void() +
  coord_equal() +
  theme(panel.background = element_rect(fill = "black")) +
  transition_time(time = i) +
  exit_fade() +
  ease_aes('linear')

# anim_save(p, filename = "gganim.gif", path = ".")

