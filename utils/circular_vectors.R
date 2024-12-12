#' circular
#' Constructor function for 'circular' S3 class
#' @param x
#' a one dimentional vector
#'
#' @return
#' a vector of class circular
#'
#' @export
#'
#' @examples
#' circular(1:3)
circular <- function(x) {
  
  if (!is.vector(x)) {
    stop("x must be a one dimentional vector")
  }
  
  # Create the object with attributes
  # obj <- matrix(x, nrow=2, dimnames = list(c("start","end"),c("x","y")))
  obj <- x
  
  # Set the class attributes
  class(obj) <- c("circular", class(x))
  
  # Return the object
  return(obj)
  
}

# generic functions for circular vectors

#' check class
#' @param x
#' an R object
#'
#' @export
is.circular <- function(x) {
  # Use the inherits function to check if 'x' is of class 'segment'
  inherits(x, "circular")
}

# Method for accessing attributes of a 'circular' vector with the $ operator
#' extract attribute
#' @param x
#' a circular vector
#' @param attr
#' attribute to extract
#' @export
`$.circular` <- function(x, attr) {
  attr(x, attr)
}

# Methods for accessing elements of a 'circular' vector by position
#' subset
#' @param x
#' an R object
#' @param i
#' index
#' @export
`[.circular` <- function(x, i) {
  if (is.numeric(i)) {
    i <- ((i-1) %% length(x))+1
    result <- NextMethod("[")
  } else {
    result <- NextMethod("[")
  }
  circular(result)
}

#' subset
#' @param x
#' an R object
#' @param i
#' index
#' @export
`[[.circular` <- function(x, i) {
  `[.circular`(x, i)
}

#' assignment by index
#' @param x
#' object from which to extract element(s) or in which to replace element(s)
#' @param i
#' index
#' @param value
#' assign to value
#' @export
`[<-.circular` <- function(x, i, value) {
  if (is.numeric(i)) {
    i <- ((i-1) %% length(x))+1
  }
  result <- NextMethod("[<-")
  circular(result)
}

#' assignment by index
#' @param x
#' object from which to extract element(s) or in which to replace element(s)
#' @param i
#' index
#' @param value
#' assign to value
#' @export
`[[<-.circular` <- function(x, i, value) {
  NextMethod("[<-")
}



# add a symbol when print
#' print
#' @param x
#' an R object
#' @param ...
#' extra args
#' @export
`print.circular` <- function(x, ...) {
  cat("Circular: ")
  NextMethod("print")
}


