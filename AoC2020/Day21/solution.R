library(tidyverse)

# Day 21: Allergen Assessment ####
input <- read_lines("AoC2020/Day21/input.txt")

get_food_list = function(input) {
  
  allergens = input %>% 
    str_match('\\(contains (.+)\\)')
  allergens = allergens[,2] %>%  
    str_split(', ', simplify = T)
  
  ingredients = input %>% 
    str_remove_all(' \\(.+\\)') %>% 
    str_split(" ", simplify = T)
  
  list(ingredients = ingredients, 
       allergens = allergens)
}

food_list = map(input,get_food_list)

# part 1 ----

allergens_set = unique(unlist(map(food_list,2)))
allergens_set=setNames(allergens_set,allergens_set)
ingredients_set = unique(unlist(map(food_list,1)))

reduce_sets = function(allergens_set, food_list){
  map(allergens_set,reduce_set,food_list)
}

reduced_sets = reduce_sets(allergens_set,food_list)

allergen_table = map(allergens_set, ~rep(T,length(ingredients_set)) %>% setNames(ingredients_set)) %>% 
  do.call(rbind,.)

map(reduced_sets, ~{
  setdiff(ingredients_set, .x)
}) %>% 
  iwalk(~{
    allergen_table[.y,.x]<<-F
  })

safe_ingredients = ingredients_set[apply(allergen_table,2,sum)==0]

count_ingredients = function(ingredients, food_list) {
  
  map_int(ingredients, function(ingredient) {
    sum(map_lgl(food_list,~ingredient %in% .x$ingredients))
  }) %>% sum()
  
}
count_ingredients(safe_ingredients, food_list)

# part 2 ----
find_allergens = function(food_list) {
  
  allergens_set = unique(unlist(map(food_list,2)))
  allergens_set=setNames(allergens_set,allergens_set)
  
  reduced_sets = reduce_sets(allergens_set,food_list)
  
  matches = allergens_set
  while (any(map_int(reduced_sets, length)==1)) {
    
    identified = reduced_sets[map_int(reduced_sets, length)==1]
    matches[names(identified)] = map_chr(identified,1)
    
    reduced_sets[names(identified)]=NULL
    reduced_sets = map(reduced_sets, ~setdiff(.x, map_chr(identified,1)))
  }
  matches
}

matches = find_allergens(food_list) 
matches[order(names(matches))] %>% 
  paste0(collapse = ',')
