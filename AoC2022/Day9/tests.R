example <- readLines("Day9/example.txt")
example2 <- readLines("Day9/example2.txt")

record_simulation(example, n_knots=2) %>%
  purrr::reduce(rbind) %>%
  unique() %>%
  nrow() == 13

record_simulation(example2, n_knots=10) %>%
  purrr::reduce(rbind) %>%
  unique() %>%
  nrow() == 36
