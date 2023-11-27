# Constructor function for 'segments' S3 class
segment <- function(x) {
  if (length(x)!=4) {
    stop("x must be a numeric vector of length 4: x, xend, y, yend")
  }
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }
  
  # Create the object with attributes
  # obj <- matrix(x, nrow=2, dimnames = list(c("start","end"),c("x","y")))
  obj <- setNames(x, c("x0","x1","y0","y1"))
  if (all(obj[c(1,3)]==obj[c(2,4)])) {stop("start and end coordinates cannot be the same")}
  
  attr(obj, "coordinates") <- obj
  slope <- (obj[4] - obj[3]) / (obj[2] - obj[1])
  attr(obj, "slope") <- slope
  attr(obj, "intercept") <- obj[3]-slope*obj[1]
  attr(obj, "length") <- sqrt((obj[4] - obj[3])^2 + (obj[2] - obj[1])^2)
  
  # Set the class attributes
  class(obj) <- c("segment", "numeric")
  
  # Return the object
  return(obj)
}

# generic functions for segments
is.segment <- function(x) {
  # Use the inherits function to check if 'x' is of class 'segment'
  inherits(x, "segment")
}

as.segment <- function(x) {
  # Use the inherits function to check if 'x' is of class 'segment'
  segment(as.numeric(x))
}

# Method for accessing attributes of a 'segment' object with the `$` operator
`$.segment` <- function(x, attr) {
  attr(x, attr)
}

print.segment <- function(x) {
  cat("from: ", x[1],",",x[3]," to: ", x[2],",",x[4],"\n", sep="")
  cat("slope:", x$slope,"\n")
  cat("intercept:", x$intercept,"\n")
  cat("length:", x$length)
}

# find intersection between 2 segments
intersect.segment <- function(s1, s2) {
  if (!is.segment(s2)) {"both s1 and s2 must be segments"}
  
  # Compute coefficients for the line equations
  A1 <- s1[4] - s1[3]
  B1 <- s1[1] - s1[2]
  C1 <- A1 * s1[1] + B1 * s1[3]
  
  A2 <- s2[4] - s2[3]
  B2 <- s2[1] - s2[2]
  C2 <- A2 * s2[1] + B2 * s2[3]
  
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
  withinS1 <- min(s1[1:2]) <= x && x <= max(s1[1:2]) && min(s1[3:4]) <= y && y <= max(s1[3:4])
  withinS2 <- min(s2[1:2]) <= x && x <= max(s2[1:2]) && min(s1[3:4]) <= y && y <= max(s2[3:4])
  
  if (withinS1 && withinS2) {
    return(c(x=x, y=y))
  } else {
    return(NULL)
  }
}

# check if a position is found on a segment
seg_include <- function(segment,x,y) {
  UseMethod("seg_include")
}

seg_include.segment <- function(segment,x,y) {
  inline <- segment$intercept + segment$slope * x - y == 0
  inrange <- dplyr::between(x, min(segment[1:2]), max(segment[1:2])) && dplyr::between(y,  min(segment[3:4]), max(segment[3:4]))
  inline && inrange
}
