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
  for (i in 1:n) {
    x <- t(apply(x,2,rev))
  }
  x
}

#' mat_mirror
#' mirror matrix vertically, horizontally, or both
#' @param x
#' matrix
#' @param axis
#' one of "V", "H", "VH","HV". V= vertical, H = Horizontal
#' @return
#' matrix
#' @export
#'
#' @examples
#' m <- matrix(1:4, nrow=2)
#' mat_mirror(m, "H")
mat_mirror <- function(x, axis = c("V", "H", "VH","HV")) {
  match.arg(axis)
  if (axis == "V") { x <- t(apply(x, 1, rev)) }
  if (axis == "H") { x <- apply(x, 2, rev) }
  if (axis %in% c("VH","HV")) { x <- mat_mirror(mat_mirror(x,"V"),"H") }
  
  return(x)
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
