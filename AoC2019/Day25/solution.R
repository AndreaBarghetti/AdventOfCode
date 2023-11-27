library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 25, .fun = read_lines)
program <- input %>% str_split(",", simplify = T) %>% as.numeric()

source('Day25/intcode_computer.R')

droid = Droid$new(program)

droid$start()

droid$action("north")
droid$action("south")
droid$action("east")
droid$action("take festive hat")
droid$action("east")
droid$action("take food ration")
droid$action("north")
droid$action("south")
droid$action("east")
droid$action("take spool of cat6")
droid$action("west")
droid$action("west")
droid$action("west")
droid$action("west")
droid$action("take hologram")
droid$action("north")
droid$action("take space heater")
droid$action("east")
droid$action("take space law space brochure")
droid$action("east")
droid$action("take tambourine")
droid$action("north")
droid$action("south")
droid$action("south")
droid$action("north")
droid$action("west")
droid$action("west")
droid$action("south")
droid$action("south")
droid$action("north")
droid$action("east")
droid$action("east")
droid$action("south")
droid$action("west")
droid$action("east")
droid$action("east")
droid$action("east")
droid$action("take fuel cell")
droid$action("east")

droid$action("inv")

items <- str_split(droid$message, '\n', simplify = T) %>% 
  str_subset("^-") %>% str_remove_all("- ")

tests = map(1:8,~combn(items,.x, FUN = list))

walk(tests, function(test) {
  walk(test, function(item) {
    
    print(setdiff(items,item))
    
    drops <- paste('drop', item)
    takes <- paste('take', item)
    walk(drops, ~droid$action(.x, print = F))
    droid$action("south", print = F)
    if(droid$computer$status == 'finshed') {
      cat(droid$message)
      stop()
    }
    walk(takes, ~droid$action(.x, print = F))
  }
  )
})

