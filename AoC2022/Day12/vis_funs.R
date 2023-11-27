# created a map with F values, and edges=T
reset_checkmap <- function(cmap) {
  map <- matrix(F,nrow = nrow(cmap),ncol = ncol(cmap))
  attributes(map)$edge <- T
  map
}

# sort possible move to prefer the one in the right direction
sort_moves <- function(moves, pos) {
  distances <- imap_dbl(setNames(moves,moves), function(.x, dir) {
    dist <- get_distance(next_pos(pos, dir))
  }) %>% sort()
  names(distances)
}

get_distance <- function(pos) {
  sum(abs(which(posmap==pos,arr.ind = T) - which(posmap==end,arr.ind = T)))
  # sqrt(sum((which(posmap==pos,arr.ind = T) - which(posmap==end,arr.ind = T))^2))
}

#
# map <- reset_map(cmap)
# checkmap <- reset_checkmap(cmap)
# pos <- start
# explored_positions <- c()
# path <- c(start) #move taken at each step
# options <- list() # moves remaining at each step
# step <- 1
# best_score <- Inf
#
# repeat {
#   checkmap[pos]<-T
#   # check if you arrived
#   if(pos==end) {
#     message("ARRIVED")
#     best_score <- length(path)
#     break
#   }
#   #find possible moves
#   # either in new pos, or from already checked pos
#   if (!pos %in% explored_positions) {
#     moves <- check_space(pos) %>% sample()
#   } else {
#     moves <- options[[pos]]
#   }
#   #if there are no moves available
#   # write T to checkmap and go back one step
#   if (length(moves)==0) {
#     path <- path[-length(path)]
#     pos <- path[length(path)]
#     step <- step+1
#     checkmap[pos]<-F
#     next
#   }
#   #otherwise
#   # write to options (except the first one that is used to move)
#   # pick the first and write it to current step
#   chosen <- moves[1]
#   options[[pos]] <- moves[-1]
#   explored_positions <- c(explored_positions, pos)
#   # move
#   pos <- next_pos(pos, chosen)
#   path <- c(path,pos)
#   #increase step
#   step <- step+1
# }
