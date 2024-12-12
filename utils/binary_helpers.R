dectobin <- function(x) {
  map_chr(x, function(x) {
    intToBits(x) %>%
      rev %>%
      as.integer %>%
      paste(collapse = '') %>%
      as.integer %>%
      as.character
  })
}
