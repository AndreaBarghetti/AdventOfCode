# dmatrix (Default matrix) return default values when
# subset for out of bound index
# note: negative indexes are also considered out of bound

# Constructor function to create a dmatrix object
dmatrix <- function(x = matrix(), default = NA) {
  if (!is.matrix(x)) {
    stop("Input must be a matrix")
  }
  
  # Set class and attributes
  structure(x,
            class = c("dmatrix", "matrix"),
            default = default)
}


# Custom method for the [ operator for dmatrix class
`[.dmatrix` <- function(x, i, j, ...) {
  # Get dimensions
  nrows <- nrow(x)
  ncols <- ncol(x)
  
  # Get the default value
  default_value <- attr(x, "default")
  
  # Handle missing indices
  if (missing(i)) i <- seq_len(nrows)
  if (missing(j)) j <- seq_len(ncols)
  
  # Ensure i and j are numeric
  i <- as.integer(i)
  j <- as.integer(j)
  
  # Handle single element access
  if (length(i) == 1 && length(j) == 1) {
    if (i >= 1 && i <= nrows && j >= 1 && j <= ncols) {
      return(NextMethod())
    } else {
      return(default_value)
    }
  }
  
  # Create result matrix
  result <- matrix(default_value, nrow = length(i), ncol = length(j))
  
  # Fill result matrix with the matrix elements or default values
  # Vectorized assignment for values within bounds
  valid_rows <- i >= 1 & i <= nrows
  valid_cols <- j >= 1 & j <= ncols
  
  # Use outer product to get valid indices and assign values
  valid_i <- i[valid_rows]
  valid_j <- j[valid_cols]
  result[valid_rows, valid_cols] <- x[valid_i, valid_j, drop = FALSE]
  
  return(result)
}

testthat::expect_equal(dm[1,1],1)
testthat::expect_equal(dm[0,1],0)
testthat::expect_equal(dm[1,4],0)
testthat::expect_equal(dmatrix(matrix(0),0)[-1:1,-1:1],matrix(0,3,3))
