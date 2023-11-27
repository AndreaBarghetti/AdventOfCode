bfs_shortest_path <- function(matrix, start, end) {
  # Define the movements: up, down, left, right
  moves <- list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1)) %>% 
    map(as.integer)
  
  # Create a queue and add the starting point
  queue <- list(start)
  visited <- matrix(FALSE, nrow(matrix), ncol(matrix))
  visited[start] <- TRUE
  visited[matrix==1] <- TRUE
  
  # Store the distances
  distances <- matrix(Inf, nrow(matrix), ncol(matrix))
  distances[start] <- 0

  # BFS loop
  while (length(queue) > 0) {
    
    # here you could sort the queue by distance,
    # in case not all steps give a dist of +1
    # queue = queue[sorted by distance]
    
    #
    current <- queue[[1]]
    queue <- queue[-1]
    
    # Check if the end is reached
    if (identical(current, end)) {
      return(distances[end])
    }
    
    # Explore neighboring cells
    for (move in moves) {
      next_pos <- current + move

      # Check boundaries and walls
      if (any(next_pos<=0) | any(dim(matrix)-next_pos<0)) {next}
      
      if ((matrix[next_pos] == 0) && (!visited[next_pos])) {
        queue <- c(queue, list(next_pos))
        visited[next_pos] <- TRUE
        
        # here instead of +1
        # you could apply a function with from,to inputs
        # that set the distance based on the from,to values
        # for example if value represent steepness, or resistance etc...
        
        distances[next_pos] <- distances[current] + 1
        
      }
    }
    
    plot_matrix(distances) +
      annotate(geom = "point", col="red",x=4,y=6)
  }
  
  # Return Inf if no path is found
  return(Inf)
}



# Example matrix
mat <- matrix(c(0, 0, 1, 0, 0, 
                1, 0, 1, 0, 1, 
                1, 0, 1, 0, 0, 
                1, 0, 1, 0, 1, 
                0, 0, 0, 0, 0), byrow = TRUE, nrow = 5)

# Start and end positions (row, column)
start <- cbind(1, 1) # Row 1, Column 1
end <- cbind(1, 5)  # Row 5, Column 5

# Find shortest path
shortest_path_length <- bfs_shortest_path(mat, start, end)
shortest_path_length
