library(tidyverse)

input = readLines('AoC2015/Day16/input.txt')
ticker_tape = readLines('AoC2015/Day16/ticker_tape') %>% 
  str_split(":") %>% 
  map(~{setNames(as.integer(.x[2]),.x[1])}) %>% unlist()

parse_sue = function(input) {
  map(input, function(l) {
    nr = str_extract(l, '\\d+') %>% as.numeric()
    l = str_split(l, ":",simplify = T, n = 2)[-1]
    l %>% str_split(",", simplify = T) %>% 
      map(str_split,":", simplify=T) %>% 
      map(~{
        setNames(as.integer(.x[2]), str_remove_all(.x[1]," "))
      }) %>% unlist()
  })
}
sues = parse_sue(input)

# part 1 ####
check_sue = function(sue, ticker_tape) {
  all(ticker_tape[names(sue)] == sue)
}

which(map_lgl(sues, check_sue, ticker_tape))

sues %>% 
  map(names) %>% unlist() %>% unique()

# part 2 ####
check_ranges = function(what, sue, ticker_tape) {
 
  if (what %in% names(sue)) {
    
    side = switch(what, 
                  trees = '>', cats = '>',
                  pomeranians = '<', goldfish =  '<')
    
    
    return(do.call(side, args = list(sue[what],ticker_tape[what])))
  } else {return(TRUE)}
}

check_sue = function(sue, ticker_tape) {

  ctpg = c('trees', 'cats', 'pomeranians', 'goldfish')
  checks = map_lgl(ctpg, check_ranges, sue, ticker_tape)
  
  if (any(!checks)) {return(F)}
  
  detected = ticker_tape[names(sue) %>% setdiff(ctpg)]
  
  all(detected == sue[names(detected)])
  
}

which(map_lgl(sues, check_sue, ticker_tape))

