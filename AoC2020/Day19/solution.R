library(tidyverse)

input19 <- read_lines("Day19/input19.txt")

# * - part 1 - *####
rules <- input19[str_detect(input19, ":")] %>%
  lapply(function(x) str_split(x, ":") %>% unlist() %>%
           str_remove_all("\""))
messages <- input19[!str_detect(input19, ":")][-1]

all_rules <- lapply(rules, function(x) x[[2]])
names(all_rules) <- lapply(rules, function(x) x[[1]])


collected <- c("0")

found <-"0"

while(length(found!=0)) {
  found <- all_rules[found] %>% 
    sapply(function(x) {
      str_extract_all(x, "[0-9]+") 
    }) %>%
    purrr::reduce(c) %>%
    unique()
  collected <<- c(collected, found) %>% unique()
  
}

needed_rules <- all_rules[collected] %>% 
  lapply(str_replace_all, "([0-9ab]+)","(\\1)") %>% 
  lapply(str_replace_all, "\\|",")|(") %>% 
  lapply(function(x) {str_c("((",x,"))")}) %>% 
  lapply(str_remove_all, " ")

main_rules2 <- needed_rules[sapply(needed_rules, str_detect, "[ab]")]
subrules2 <- needed_rules[!sapply(needed_rules, str_detect, "[ab]")]

decoded_rules <- list()
main_rules2
subrules2


decnames<-c()
while(length(main_rules2)!=0){
  
  iwalk(main_rules2, function(rulevalue, ruleN) {
    decnames <<- c(decnames, ruleN)
    decoded_rules[ruleN] <<- rulevalue
    main_rules2[ruleN] <<- NULL
    subrules2<<-lapply(subrules2, function(value) {
      str_replace_all(value, str_c("\\(",ruleN,"\\)"),  rulevalue)
    })
    main_rules2 <<- c(main_rules2, subrules2[!subrules2 %>%
                                               sapply(function(x) {
                                                 str_detect(x, "[0-9]")
                                               })])
    subrules2[!(subrules2 %>% sapply(str_detect, "[0-9]"))] <<- NULL
    
  })
}

decnames
main_rules2
subrules2
decoded_rules

decoded_rules <- decoded_rules %>% 
  lapply(str_remove_all," ") %>% 
  lapply(str_replace_all,"\\(([ab]+)\\)","\\1") %>% 
  lapply(str_replace_all,"\\(([ab]+)\\)","\\1")

all_rules[["0"]]
decoded_rules[["0"]]
decoded_rules[["0"]] == str_c("((",decoded_rules[["8"]],decoded_rules[["11"]],"))")

rule0 <- str_c("^",decoded_rules[["0"]],"$")

sum(sapply(messages, str_detect, rule0))

# * - part 2 - *####
all_rules[["8"]] <- " 42 | 42 8"
all_rules[["8"]] <- " 42+"

all_rules[["11"]] <- " 42 31 | 42 11 31"
all_rules[["11"]] <- " 42 31 | 42 42 31 31 | 42 42 42 31 31 31 | 42 42 42 42 31 31 31 31"

# and then the same... 
