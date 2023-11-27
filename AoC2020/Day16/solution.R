library(tidyverse)

input16 <- read_lines("Day16/input16.txt")

# * - part 1 - *####
rules <- input16[1:(grep(input16, pattern = "your ticket:")-2)]
my_ticket <- input16[grep(input16, pattern = "your ticket:")+1] %>%
  str_split(",") %>% unlist() %>% as.integer() 
all_tickets <- input16[-c(1:(grep(input16, pattern = "nearby tickets:")+1))] %>%
  str_split(",") %>% lapply(as.integer)

all_ranges <- str_match_all(rules, "[0-9]+-[0-9]+") %>% 
  unlist() %>% lapply(function(x) {c(str_match(x,"^[0-9]+") %>% as.integer(), 
                                     str_match(x,"[0-9]+$") %>% as.integer())})

all_values <- lapply(all_ranges, function(range) {c(range[1]:range[2])}) %>% unlist() %>% unique() %>% sort()

error_rate <- sum(sapply(all_tickets, function(ticket) {sum(ticket[!ticket %in% all_values])}))

# * - part 2 - *####
all_ranges2 <- str_match_all(rules, "[0-9]+-[0-9]+") %>% 
  lapply(function(x) { unlist(x) %>% lapply(function(x) {c(str_match(x,"^[0-9]+") %>% as.integer(), 
                                                           str_match(x,"[0-9]+$") %>% as.integer())})})

valid_tickets <- all_tickets[sapply(all_tickets, function(ticket) {all(ticket %in% all_values)})]

rules_names <- str_match(rules, "^(.*):")[,2]
names(all_ranges2) <- rules_names

possible <- 1:20

matrix <- sapply(seq_along(all_ranges2), function(f) {
  sapply(possible, function(i) {
    all(sapply(valid_tickets, function(x) x[i]) %>% between(all_ranges2[[f]][[1]][1],all_ranges2[[f]][[1]][2]) |
          sapply(valid_tickets, function(x) x[i]) %>% between(all_ranges2[[f]][[2]][1],all_ranges2[[f]][[2]][2]))
  })
}) 

df <- matrix %>% as_tibble()
colnames(df) <- c(rules_names)

#I could not code the sulution, I had to solve it like a sudoku
df %>%
  rownames_to_column(var = "f") %>% 
  mutate_all(.,as.integer) %>%
  gather(field, value, 2:21) %>%
  ggplot(aes(x=field, y=f))+
  geom_text(aes(label=value))+
  theme_bw() +
  scale_x_discrete(position = "top") + 
  scale_y_reverse(breaks = 1:20) +
  theme(axis.text.x = element_text(angle=90, hjust=0, vjust=.5))

results <- list("departure date" = 2,
                "departure time"=3,
                "departure location" =7,
                "departure track" = 13,
                "departure platform" = 15,
                "departure station"=17)

my_ticket[results %>% unlist()] %>% prod()
