#' mat_to_tibble
#'
#' @param matrix
#' matrix
#' @return
#' tibble
#' @export
#'
mat_to_tibble <- function(matrix) {
  df <- reshape2::melt(mat_mirror(t(matrix),"V"),
                       value.name = c("value"))
  dplyr::rename(df, x = "Var1", y = "Var2")
}

#' plot a matrix
#'
#' @param matrix
#' matrix
#' @return
#' plot
#' @export
#'
plot_matrix <- function(matrix) {
  x<-y<-value<-NULL
  space <- mat_to_tibble(matrix)
  
  ggplot2::ggplot(space, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), show.legend = F) +
    ggplot2::theme_void() +
    ggplot2::coord_equal()
}
