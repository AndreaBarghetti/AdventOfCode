#' segment
#' Constructor function for 'segments' S3 class
#' @param x
#' a numeric vector of length 4, with start/end coordinates of the segment: x1, y1, x2, y2
#' @return
#' segment
#' @export
#'
#' @examples
#' segment(1:4)
segment <- function(x) {
  if (length(x)!=4) {
    stop("x must be a numeric vector of length 4: x1, y1, x2, y2")
  }
  x=as.numeric(x)
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  
  # Create the object with attributes
  # obj <- matrix(x, nrow=2, dimnames = list(c("start","end"),c("x","y")))
  obj <- stats::setNames(x, c("x1","y1","x2","y2"))
  if (all(obj[c(1,2)]==obj[c(3,4)])) {stop("start and end coordinates cannot be the same")}
  
  attr(obj, "coordinates") <- obj
  slope <- (obj[4] - obj[2]) / (obj[3] - obj[1])
  attr(obj, "slope") <- slope
  attr(obj, "intercept") <- obj[2]-slope*obj[1]
  attr(obj, "length") <- sqrt((obj[4] - obj[2])^2 + (obj[3] - obj[1])^2)
  
  # Set the class attributes
  class(obj) <- c("segment", "numeric")
  
  # Return the object
  return(obj)
}


#generic functions for segment

#' is.segment
#' @param x
#' x
#' @return
#' logical
#' @export
is.segment <- function(x) {
  # Use the inherits function to check if 'x' is of class 'segment'
  inherits(x, "segment")
}

#' as.segment
#' @param x
#' x
#' @return
#' segment
#' @export
as.segment <- function(x) {
  # Use the inherits function to check if 'x' is of class 'segment'
  segment(as.numeric(x))
}

#' extract
#' Method for accessing attributes of a 'segment' object with the $ operator
#' @param x
#' x
#' @param attr
#' attribute
#' @return
#' logical
#' @export
`$.segment` <- function(x, attr) {
  attr(x, attr)
}

#' print
#' @param x
#' x
#' @param ...
#' other params
#' attribute
#' @return
#' x
#' @export
print.segment <- function(x, ...) {
  cat("from: ", x[1],",",x[2]," to: ", x[3],",",x[4],"\n", sep="")
  cat("slope:", x$slope,"\n")
  cat("intercept:", x$intercept,"\n")
  cat("length:", x$length)
  invisible(x)
}

#' intersect segments
#' find intersection between 2 segments
#' @param s1
#' segment 1
#' @param s2
#' segment 2
#' @return
#' x,y vector
#' @export
#'
intersect.segment <- function(s1, s2, esclude_extremes=FALSE) {
  if (!is.segment(s2)) {"both s1 and s2 must be segments"}
  
  if (esclude_extremes) {
    if ( seg_include(s2, s1[1], s1[2]) ) { return(NULL) }
    if ( seg_include(s2, s1[3], s1[4]) ) { return(NULL) }
    if ( seg_include(s1, s2[1], s2[2]) ) { return(NULL) }
    if ( seg_include(s1, s2[3], s2[4]) ) { return(NULL) }
  }
  
  # Compute coefficients for the line equations
  A1 <- s1[4] - s1[2]
  B1 <- s1[1] - s1[3]
  C1 <- A1 * s1[1] + B1 * s1[2]
  
  A2 <- s2[4] - s2[2]
  B2 <- s2[1] - s2[3]
  C2 <- A2 * s2[1] + B2 * s2[2]
  
  # Compute the determinant
  det <- A1 * B2 - A2 * B1
  
  # If the determinant is zero, lines are parallel or coincident
  if (det == 0) {
    return(NULL)
  }
  
  # Solve the system to find the intersection point
  x <- (B2 * C1 - B1 * C2) / det
  y <- (A1 * C2 - A2 * C1) / det
  
  # Check if the intersection point lies within both segments
  withinS1 <- min(s1[c(1,3)]) <= x && x <= max(s1[c(1,3)]) && min(s1[c(2,4)]) <= y && y <= max(s1[c(2,4)])
  withinS2 <- min(s2[c(1,3)]) <= x && x <= max(s2[c(1,3)]) && min(s2[c(2,4)]) <= y && y <= max(s2[c(2,4)])
  
  if (withinS1 && withinS2) {
    return(c(x=x, y=y))
  } else {
    return(NULL)
  }
}

intersect <- function(s1, s2, esclude_extremes = F) {
  UseMethod("intersect")
}


#' Title
#' check if a position is found on a segment
#' @param segment
#' segment
#' @param x
#' x
#' @param y
#' y
#' @param esclude_extremes
#' logical
#' @return
#' logical
#' @export
#'
seg_include <- function(segment, x, y, esclude_extremes=FALSE) {
  UseMethod("seg_include")
}

#' seg_include
#' check if a position is found on a segment
#' must fix. doesn't work for vertical and horizontal segments
#' @param segment
#' segment
#' @param x
#' x
#' @param y
#' y
#' @param esclude_extremes
#' logical
#' @return
#' logical
#' @export
#'
seg_include.segment <- function(segment, x, y, esclude_extremes=FALSE) {
  
  if (abs(segment$slope)!=Inf) {
    inline <- isTRUE(all.equal(target = unname(segment$intercept + segment$slope * x - y), current = 0))
  } else {
    inline <- x==segment[1]
  }
  
  inrange <- dplyr::between(x, min(segment[c(1,3)]), max(segment[c(1,3)]))  && dplyr::between(y, min(segment[c(2,4)]), max(segment[c(2,4)]))
  
  if (esclude_extremes) {
    if (x == segment[1] && y == segment[2]) {inrange <- FALSE}
    if (x == segment[3] && y == segment[4]) {inrange <- FALSE}
  }
  inline && inrange
}


