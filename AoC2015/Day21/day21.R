library(tidyverse)

input = read_lines('AoC2015/Day21/input.txt')

parse_shop = function(file) {
  
  shop = read_lines(file)
  seps = which(shop=='')
  col_names=c('Item','C','D','A')
  weapons = read_table(file, skip=1, n_max = seps[1]-2, col_names = col_names)
  armors = read_table(file, skip=seps[1]+1, n_max = seps[2]-seps[1]-2, col_names = col_names)
  rings = read_table(file, skip=seps[2]+1, col_names = col_names)
  list(weapons=weapons,
       armors=armors,
       rings=rings)
}

shop = parse_shop('AoC2015/Day21/inventory.txt')

boss = str_extract_all(input,'\\d+') %>% as.numeric() %>% setNames(nm = c('HP','D','A'))

you = setNames(c(100,0,0), nm = c('HP','D','A'))

# part 1 ----
fight = function(you, boss) {
  # round = 0
  repeat {
    # round = round+1
    boss['HP'] = boss['HP']-max(1, you['D']-boss['A'])
    if (boss['HP']<=0) {return(TRUE)}
    
    you['HP'] = you['HP']-max(1, boss['D']-you['A'])
    if (you['HP']<=0) {return(FALSE)}
  }
}

try_all_items = function(shop) {

  min_cost = Inf
  
  empty_item = tibble(Item='nothing',C=0,D=0,A=0)
  ring_combs = rbind(c(0,0),t(combn(c(0:nrow(shop$rings)),2)))
  
  for (w in 1:nrow(shop$weapons)) {
    for (a in 0:nrow(shop$armors)) {
      for (r in 1:nrow(ring_combs)) {
          weapon = shop$weapons[w,]
          armor = shop$armors[a,]
          r1 = ring_combs[r,1]
          r2 = ring_combs[r,2]
          ring1 = shop$rings[r1,]
          ring2 = shop$rings[r2,]
          
          inventory = bind_rows(weapon, armor, ring1, ring2)
          inv = unlist(inventory %>% summarise(across(where(is.numeric), sum)))
          
          f = fight(you + c(0,inv[c('D','A')]), boss)
          if (f) {min_cost = min(inv['C'], min_cost)}
      }
    }
  }
  min_cost
}

try_all_items(shop)

# part 2 ----
try_all_items2 = function(shop) {
  
  max_cost = 0
  
  empty_item = tibble(Item='nothing',C=0,D=0,A=0)
  ring_combs = rbind(c(0,0),t(combn(c(0:nrow(shop$rings)),2)))
  
  for (w in 1:nrow(shop$weapons)) {
    for (a in 0:nrow(shop$armors)) {
      for (r in 1:nrow(ring_combs)) {
        weapon = shop$weapons[w,]
        armor = shop$armors[a,]
        r1 = ring_combs[r,1]
        r2 = ring_combs[r,2]
        ring1 = shop$rings[r1,]
        ring2 = shop$rings[r2,]
        
        inventory = bind_rows(weapon, armor, ring1, ring2)
        inv = unlist(inventory %>% summarise(across(where(is.numeric), sum)))
        
        f = fight(you + c(0,inv[c('D','A')]), boss)
        if (!f) {max_cost = max(inv['C'], max_cost)}
      }
    }
  }
  max_cost
}

try_all_items2(shop)
