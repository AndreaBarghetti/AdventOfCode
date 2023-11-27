library(tidyverse)

# Day 17: Conway Cubes ####
input17 <- read_lines("Day17/input17.txt")

cubes <- input17 %>%
  lapply(function(x) {
    x %>% 
      str_replace_all(c("\\."="0", "#"="1")) %>% 
      str_split("") %>%
      unlist() %>%
      as.integer()
  })

# * - part 1 - *####

tidy_space <- tibble(x=-12:12, y=-12:12, z=-12:12) %>%
  expand(x,y,z) %>%
  mutate(state=0)

# inizialize space:
for (y in seq_along(cubes)) {
  for (x in seq_along(cubes[[y]])) {
    tidy_space$state[tidy_space$x==x & 
                       tidy_space$y==y &
                       tidy_space$z==0] <- cubes[[y]][x]
  }
}


simulate_expansion <- function(tidy_space, cycles) {
  
  for (i in 1:cycles) {
    tidy_charge <- pmap(tidy_space %>%
                          filter(state==1), function(x,y,z,...){
                            near <- tibble(
                              x=c((x-1):(x+1)),
                              y=c((y-1):(y+1)),
                              z=c((z-1):(z+1))
                            ) %>% 
                              expand(x,y,z) %>%
                              mutate(charge=1)
                            return(near)
                          }) %>%
      reduce(bind_rows) %>%
      group_by(x,y,z) %>%
      summarise(charge=sum(charge))
    
    tidy_space <- left_join(tidy_space, tidy_charge) %>%
      mutate(charge= case_when(is.na(charge)~0, 
                               T~charge),
             state=case_when(state==1 & charge%in% c(3,4) ~ 1,
                             state==1 & !charge%in% c(3,4) ~ 0,
                             state==0 & charge==3 ~ 1,
                             state==0 & charge!=3 ~ 0)) %>%
      select(-charge)
  }
  return(sum(tidy_space$state))
}

simulate_expansion(tidy_space, 6)


# * - part 2 - *####
tidy_space <- tibble(x=-13:13, y=-13:13, z=-13:13, w=-13:13) %>%
  expand(x,y,z,w) %>%
  mutate(state=0)

# inizialize space:
for (y in seq_along(cubes)) {
  for (x in seq_along(cubes[[y]])) {
    tidy_space$state[tidy_space$x==x & 
                       tidy_space$y==y &
                       tidy_space$z==0 &
                       tidy_space$w==0] <- cubes[[y]][x]
  }
}

simulate_expansion <- function(tidy_space, cycles) {
  
  for (i in 1:cycles) {
    tidy_charge <- pmap(tidy_space %>%
                          filter(state==1), function(x,y,z,w,...){
                            near <- tibble(
                              x=c((x-1):(x+1)),
                              y=c((y-1):(y+1)),
                              z=c((z-1):(z+1)),
                              w=c((w-1):(w+1))
                            ) %>% 
                              expand(x,y,z,w) %>%
                              mutate(charge=1)
                            return(near)
                          }) %>%
      reduce(bind_rows) %>%
      group_by(x,y,z,w) %>%
      summarise(charge=sum(charge), `.groups`="drop")
    
    tidy_space <- left_join(tidy_space, 
                            tidy_charge, 
                            by=c("x","y","z","w")) %>%
      mutate(charge= case_when(is.na(charge)~0, 
                               T~charge),
             state=case_when(state==1 & charge%in% c(3,4) ~ 1,
                             state==1 & !charge%in% c(3,4) ~ 0,
                             state==0 & charge==3 ~ 1,
                             state==0 & charge!=3 ~ 0)) %>%
      select(-charge)
  }
  return(sum(tidy_space$state))
}

simulate_expansion(tidy_space, 6)
