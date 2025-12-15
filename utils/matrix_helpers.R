#' mat_rotate
#' rotate matrix 90, 180, 270 ... clockwise
#' @param x
#' matrix
#' @param n
#' number of clockwise 90° rotations
#' @return
#' matrix
#'
#' @export
#'
#' @examples
#' m <- matrix(1:4, nrow=2)
#' mat_rotate(m, n=3)
mat_rotate <- function(x, n) {
  n=as.integer(n)
  if (!"matrix" %in% class(x)) {stop("x must be a matrix")}
  if (n==0) {return(x)}
  
  rotate_90 <- function(m) {
    t(m)[, ncol(t(m)):1, drop = FALSE]
  }
  
  for (i in 1:n) {
    x <- rotate_90(x)
  }
  x
}

#' mat_mirror
#' mirror matrix vertically, horizontally, or both
#' @param x
#' matrix
#' @param axis
#' one of "V", "H". V= vertical, H = Horizontal
#' @return
#' matrix
#' @export
#'
#' @examples
#' m <- matrix(1:4, nrow=2)
#' mat_mirror(m, "H")
mat_mirror <- function(x, axis = c("V", "H")) {
  if (!is.matrix(x)) {stop('mat must be a matrix')}
  
  axis = match.arg(axis)
  
  nr <- nrow(x)
  nc <- ncol(x)
  
  if (axis == "H") {
    return(x[, nc:1, drop = FALSE])
  } else if (axis == "V") {
    return(x[nr:1, , drop = FALSE])
  }
}

#' mat_expand
#' expand matrix in either directions
#' @param x
#' matrix
#' @param directions
#' one or more directions of "L","R","U","D"
#' @param n
#' number of extra rows/columns to add
#' @param fill
#' fill extra rows with
#' @return
#' matrix
#' @export
#'
#' @examples
#' m <- matrix(1:4, nrow=2)
#' mat_expand(m, directions = c("U","D"), n=2, fill=0)
mat_expand <-  function(x,
                        directions = c("L","R","U","D"),
                        n,
                        fill) {
  
  if ("L" %in% directions) {
    extra <- matrix(fill, nrow(x), ncol = n)
    x <- cbind(extra, x)
  }
  if ("R" %in% directions) {
    extra <- matrix(fill, nrow(x), ncol = n)
    x <- cbind(x, extra)
  }
  if ("U" %in% directions) {
    extra <- matrix(fill, ncol(x), nrow = n)
    x <- rbind(extra, x)
  }
  if ("D" %in% directions) {
    extra <- matrix(fill, ncol(x), nrow = n)
    x <- rbind(x, extra)
  }
  return(x)
}


#' print_matrix
#'
#' @param matrix 
#'
#' @return
#' @export
#'
#' @examples
print_matrix = function(matrix) {
  matrix %>% 
    apply(1, function(x){
      cat(paste0(x, collapse = ""))
      cat("\n")
    })
  cat("\n")
  invisible(matrix)
}

#' mat_all_rotations
#'
#' make a list with all possible rotations and mirrors of the matrix
#'
#' @param x matrix
#' 
#' @param mirror if allow flipping the matrix 
#' 
#' @param rotate if allow rotating the matrix 
#'
#' @param unique return only the unique matrices
#'
#' @return
#' a list of matrices 
#' @export
#'
mat_all_rotations = function(x, 
                             mirror = T, 
                             rotate = T, 
                             unique = T) {
  
  if (!is.matrix(x)) {stop('mat must be a matrix')}
  
  res = list(x)
  
  if (!(mirror | rotate)) {return(res)}
  
  if (mirror) {
    flipped = mat_mirror(x, 'H')
    res = list(x, flipped)
  }
  
  if (rotate) {
    res = map(res, \(m){
      map(c(0,1,2,3), ~mat_rotate(m, .x))
    }) %>% unlist(recursive = F)
  }
  
  if (unique) {res = unique(res)}
  
  res
} 

