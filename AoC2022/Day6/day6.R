library(tidyverse)

input <- readLines("Day6/input.txt")

find_signal_start <- function(signal, min_size) {
  for (i in 1:nchar(signal)) {
    str <- str_sub(signal,i,i+min_size-1)
    if (str_detect(str, "(.).*\\1", negate = T)) {
      return(i+min_size-1)
    }
  }
}

# Part 1 ####
find_signal_start(input,4)

# Part 2 ####
find_signal_start(input,14)


# Visualization ####
library(animation)

df <- input %>%
  str_split("") %>%
  unlist() %>%
  tibble(char=.) %>%
  mutate(pos=row_number())

plot_scan <- function(i, size=14, start_at=2800) {

  df <- df[start_at:3200,]

  df1 <- df %>%
  slice(1:size+i) %>%
  mutate(repeated = duplicated(char))

  if (all(!df1$repeated)) {stop<<-T}

  df1 %>%
  bind_rows(df[(i+size+1):(i+50),]) %>%
  ggplot(aes(x=pos, y="",col=!repeated, label=char)) +
  geom_text(show.legend = F, size=10) +
  theme_void() +
  # ggtitle(label = i+size-1+start_at) +
  scale_color_manual(values = c("red", "blue"))
}

stop<-F
save_animation <- function(n) {

  saveGIF({
    for (i in 1:n) {
      if(!stop){x<-i}
      print(plot_scan(x))
    }
  },
  loop=T,
  interval=0.05,
  ani.width = 1000,
  ani.height = 100,
  movie.name = "animation.gif",
  outdir = "Day6/",
  clean = T
  )
}

save_animation(300)
