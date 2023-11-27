library(tidyverse)
input <- readLines("Day10/input.txt")

# part 1 ####
read_input <- function(input) {
  fun <- str_extract(input, "\\w+")
  val <- str_extract(input, "[-]*\\d+") %>% as.integer()
  list(fun=fun, val=val)
}

run_program <- function(input) {
  X <- 1
  cicle <- 1
  values <- 0
  for (inp in input) {
    program <- read_input(inp)

    if (program$fun == "addx") {
      cicle <- cicle + 1
      if ((cicle-20)%%40==0) {values <- c(values,(X*cicle))}
      cicle <- cicle + 1
      X <- X + program$val
      if ((cicle-20)%%40==0) {values <- c(values,(X*cicle))}
    }

    if (program$fun == "noop") {
      cicle <- cicle +1
      if ((cicle-20)%%40==0) {values <- c(values,(X*cicle))}
    }
  }
  values
}

sum(run_program(input))

# part 2 ####
write_screen <- function(screen, cicle, sprite) {
    if ((((cicle-1) %% 40)+1) %in% sprite) {
      x <-"#"
    } else {
      x <- "."
    }
  c(screen,x)
}

render_screen <- function(screen) {
  screen <- matrix(screen, ncol=6) %>% t()
  invisible({apply(screen,1,function(x) {
    cat(str_c(x, collapse = ""))
    cat("\n")
  })})
}

run_program2 <- function(input) {
  X <- 2
  cicle <- 1
  screen <- c()

  for (inp in input) {
    program <- read_input(inp)

    sprite <- (X-1):(X+1)

    if (program$fun == "addx") {
      # screen <- write_screen(screen = screen, cicle = cicle, sprite = sprite)
      screen <- write_screen(screen, cicle, sprite)

      cicle <- cicle + 1

      # screen <- write_screen(screen = screen, cicle = cicle, sprite = sprite)
      screen <- write_screen(screen, cicle, sprite)

      cicle <- cicle + 1

      X <- X + program$val
    }

    if (program$fun == "noop") {
      # screen <- write_screen(screen = screen, cicle = cicle, sprite = sprite)
      screen <- write_screen(screen, cicle, sprite)

      cicle <- cicle +1
    }
  }
  render_screen(screen)
}

run_program2(input)

# Visualization ####
library(animation)
plot_screen <- function(screen, X, cicle) {
  Xmin <- X-1.5
  Xmax <- X+1.5
  c_xmin <- (((cicle-1) %% 40)+1)-.5
  c_xmax <- (((cicle-1) %% 40)+1)+.5
  c_ymin <- (cicle%/%40)+1-.5
  c_ymax <- (cicle%/%40)+1+.5

  screen[(length(screen)+1):240]<-NA_character_
  screen <- matrix(screen, ncol=6) %>% t()

  space <- reshape2::melt(t(screen))
  space %>%
    ggplot(aes(x=Var1,y=-Var2)) +
    geom_tile(aes(fill=value),show.legend=F) +
    theme_void() +
    scale_fill_manual(values=c("white","black","gray")) +
    annotate(geom="rect", fill="transparent", col="red",xmin = Xmin,xmax=Xmax,ymin=-6.5,ymax=-.5) +
    annotate(geom="rect", fill="green", col="green", alpha=.5, xmin = c_xmin,xmax=c_xmax,ymin=-c_ymin,ymax=-c_ymax) +
    coord_cartesian(xlim=c(0.5,40.5), ylim=c(-6.5,-.5))
}

write_screen <- function(screen, cicle, sprite) {
  if ((((cicle-1) %% 40)+1) %in% sprite) {
    x <-"#"
  } else {
    x <- "."
  }
  X <- sprite[2]
  print(plot_screen(screen,X,cicle))
  c(screen,x)
}

saveGIF({
  run_program2(input)
},
loop=T,
interval=0.05,
ani.width = 500,
ani.height = 75,
movie.name = "animation.gif",
outdir = "Day10/",
clean = T
)

