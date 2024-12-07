# Constructor for dmatrix
dmatrix <- function(x = numeric(), nrow, ncol, default = NA) {
  m <- matrix(x, nrow = nrow, ncol = ncol)
  
  if (storage.mode(m) != storage.mode(default)) {
    stop(sprintf("The default value must have the same type as the matrix elements. Expected '%s', got '%s'.",
                 storage.mode(m), storage.mode(default)))
  }
  
  attr(m, "default") <- default
  class(m) <- c("dmatrix", "matrix")
  m
}

as_dmatrix <- function(x, default = NA) {
  if (!is.matrix(x)) {
    stop("`x` must be a matrix.")
  }
  
  # Check that the default value has the same type as the matrix elements
  if (storage.mode(x) != storage.mode(default)) {
    stop(sprintf("The default value must have the same type as the matrix elements. Expected '%s', got '%s'.",
                 storage.mode(x), storage.mode(default)))
  }
  
  attr(x, "default") <- default
  class(x) <- c("dmatrix", "matrix")
  x
}


# Indexing method for dmatrix
`[.dmatrix` <- function(x, i, j, drop = TRUE) {
  default_val <- attr(x, "default")
  nr <- nrow(x)
  nc <- ncol(x)
  
  # Convert to base matrix to avoid recursive calls
  base_x <- unclass(x)
  
  # Handle missing indices
  if (missing(i) && missing(j)) {
    # x[] just returns the whole object
    return(x)
  } else if (missing(i)) {
    i <- seq_len(nr)
  } else if (missing(j)) {
    j <- seq_len(nc)
  }
  
  i <- as.integer(i)
  j <- as.integer(j)
  
  valid_i <- i[i >= 1 & i <= nr]
  valid_j <- j[j >= 1 & j <= nc]
  
  # Prepare a result matrix filled with default values
  res <- matrix(default_val, nrow = length(i), ncol = length(j))
  
  if (length(valid_i) > 0 && length(valid_j) > 0) {
    # Index the underlying matrix (no recursion here)
    submat <- base_x[valid_i, valid_j, drop = FALSE]
    
    # Map back to positions in res
    row_map <- match(valid_i, i)
    col_map <- match(valid_j, j)
    res[row_map, col_map] <- submat
  }
  
  if (drop && (length(i) == 1 || length(j) == 1)) {
    return(as.vector(res))
  } else {
    # Retain class information
    # If you want the subset also to remain a dmatrix, you can keep it.
    # Otherwise, if you only want the final returned object as a matrix, omit the next line.
    # To return a dmatrix with the same default:
    attr(res, "default") <- default_val
    class(res) <- c("dmatrix", "matrix")
    return(res)
  }
}

dm <- dmatrix(1:9, nrow = 3, ncol = 3, default = as.integer(0))

testthat::expect_equal(dm[1,1],1)
testthat::expect_equal(dm[0,1],0)
testthat::expect_equal(dm[1,4],0)
testthat::expect_equal(dm[-1:1,4],integer(length = 3))
testthat::expect_equal(dmatrix(0,1,1,0)[-1:1,-1:1],dmatrix(0,3,3,0))

