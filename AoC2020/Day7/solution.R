library(tidyverse)

input7 <- readLines("Day7/input7.txt")

# * - part 1 - *####
rules <- lapply(input7, function(line) {
  rule <- line %>%
    str_remove("\\.$") %>%
    str_split(" bags contain ") %>%
    unlist()
  names(rule) <- c("bag","inside")
  return(rule %>% as.list())
})

bags <- sapply(rules, function(x) {
  x$bag
})

insides <- sapply(rules, function(x) {
  x$inside
})

insides2 <- lapply(insides, function(x) {
  what <- x %>% str_split(", ") %>% unlist()
  howmany <- what %>% sapply(parse_number)
  howmany <- ifelse(is.na(howmany),0,howmany)
  names(howmany) <- what %>% str_remove_all(c("[0-9] | bags| bag"))
  return(howmany)
})

names(insides2) <- bags

bags_df <- lapply(bags, function(bag) {
  res <- insides2[[bag]] %>%
    as.list() %>% 
    as_tibble() %>%
    mutate(bag=bag)
  return(res)
}) %>% purrr::reduce(bind_rows) %>%
  select(bag, everything())

tidy_bags <- bags_df %>% 
  gather(contain, number, 2:ncol(bags_df)) %>%
  arrange(bag) %>%
  filter(!is.na(number))

lev1 <- tidy_bags %>%
  filter(contain=="shiny gold") %>%
  pull(bag) %>%
  unique()

possible_bags <- lev1
lev <- lev1

while(length(lev)>0) {
  lev <- tidy_bags %>%
    filter(contain %in% lev) %>%
    pull(bag) %>%
    unique()
  possible_bags <- c(possible_bags, lev)
  if(length(lev)==0) {break()}
}

length(possible_bags %>% unique())

# * - part 2 - *####
n <- 0

test <- tidy_bags %>%
  filter(bag=="shiny gold") %>%
  select(contain, number)

while(nrow(test)>0) {
  n <- n + sum(test$number)
  print(n)
  test <- test %>%
    rename(bag=contain) %>%
    left_join(tidy_bags, by="bag") %>%
    mutate(number=number.x*number.y) %>%
    select(contain, number) %>%
    filter(contain!="no other")
  print(test)}
n
#
