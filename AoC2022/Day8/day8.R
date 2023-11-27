library(tidyverse)

input <- readLines("Day8/input.txt")

forest <- matrix(as.integer(str_split(input, "", simplify = T)),    # Convert to numeric matrix
                 nrow = length(input))

# part 1 ####
spot_treesFw <- function(trees) {
  sapply(seq_along(trees),function(i){
    if(i==1) T
    else trees[i]>max(trees[1:(i-1)])
  })
}

spot_treesRv <- function(trees) {
  rev(sapply(seq_along(trees),function(i){
    if(i==1) T
    else rev(trees)[i]>max(rev(trees)[1:(i-1)])
  }))
}

spot_trees <- function(trees) {
  Fw <- spot_treesFw(trees)
  Rv <- spot_treesRv(trees)
  Fw|Rv
}

spot_trees2D <- function(forest) {
  leftright <- t(apply(forest,1,spot_trees))
  topbottom <- apply(forest,2,spot_trees)
  leftright|topbottom
}

sum(spot_trees2D(forest))

# part 2 ####
get_viewFw <- function(trees) {

  imax <- length(trees)

  view <- sapply(seq_along(trees),function(i){
    check_trees <- trees[(i+1):imax]
    my_tree <- trees[i]

    if (i==imax) {0} # edge case
    else if (trees[i]>max(check_trees)) { imax-i } # view until edge
    else { min(which(check_trees>=my_tree)) } # view until tree
  })

  view
}

get_viewRv <- function(trees) {
  rev(get_viewFw(rev(trees)))
}

get_view <- function(trees) {
  Fw <- get_viewFw(trees)
  Rv <- get_viewRv(trees)
  Fw*Rv
}

spot_trees2D2 <- function(forest) {
  h_view <- t(apply(forest,1,get_view))
  v_view <- (apply(forest,2,get_view))
  h_view * v_view
}

max(spot_trees2D2(forest))

# Visualization ####
library(animation)

plot_matrix <- function(matrix) {
  space <- reshape2::melt(matrix)
  space %>%
    ggplot(aes(x=Var1,y=Var2)) +
    geom_tile(aes(fill=value),show.legend=F) +
    theme_void()
}

save_animation <- function(forest) {

  height_plot <- plot_matrix(forest) +
    scale_fill_gradient2(low = "black", mid = "green",high = "white", midpoint = 7) +
    ggtitle("Show: tree height")

  visible_plot <- plot_matrix(spot_trees2D(forest)) +
    scale_fill_manual(values = c("black", "dark green")) +
    ggtitle("Show: visible trees")

  view_plot <- plot_matrix(spot_trees2D2(forest)) +
    scale_fill_gradient2(low = "black", mid = "green",high = "white", midpoint=190000) +
    ggtitle("Show: view score")

  treehouse_plot <- (max(spot_trees2D2(forest)) == spot_trees2D2(forest)) %>%
    plot_matrix() +
    scale_fill_manual(values = c("black", "white")) +
    ggtitle("Show: selected tree")

  saveGIF({
    print(height_plot)
    print(visible_plot)
    print(view_plot)
    print(treehouse_plot)

  },
  loop=T,
  interval=2,
  ani.width = 500,
  ani.height = 500,
  movie.name = "animation.gif",
  outdir = "Day8/",
  clean = T
  )
}

save_animation(forest)

make_random_forest <- function(nrow=99,ncol=99) {
  matrix( sample(1:9,nrow*ncol, replace = T), ncol=ncol)
}
