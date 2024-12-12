str_rev <- function(s) {
  char_vector <- str_split(s, "", simplify = T)
  reversed_vector <- apply(char_vector,1,rev)
  apply(reversed_vector,2,paste, collapse = "")
}


