library(tidyverse)
input <- read_lines("AoC2023/Day3/input.txt")

# Part 1 ####
count_parts <- function(input) {
  
  nums <- str_extract_all(input, "\\d+") %>% map(as.integer)
  nums_loc <- str_locate_all(input, "\\d+")
  symbols_loc <- str_locate_all(input, "[^\\.\\d]+")
  
  def_mat = list(matrix(numeric(0), ncol=2,dimnames = list(NULL, c("start", "end"))))
  
  pmap(list(nums, 
            nums_loc, 
            symbols_loc,
            lead(symbols_loc, default = def_mat), 
            lag(symbols_loc, default = def_mat)), function(num,
                                         num_loc, 
                                         symbol_loc,
                                         symbol_loc_down,
                                         symbol_loc_up
                                         ) {
              
    num[map2_lgl(num_loc[,"start"]-1,num_loc[,"end"]+1, ~{
      c(between(symbol_loc[,"start"], .x,.y),
        between(symbol_loc_up[,"start"], .x,.y),
        between(symbol_loc_down[,"start"], .x,.y)) %>% any()
    })]
  })
  
}

count_parts(input) %>% unlist() %>% sum()

# Part 2 ####
get_nums <- function(lines, pos) {
  map(lines, function(line) {
    locs = str_locate_all(line, "\\d+")[[1]]
    keep = apply(locs,1,function(x) {between(pos,x[1]-1,x[2]+1)})
    str_sub(line, locs[,1], locs[,2])[keep]
  }) %>% unlist() %>% na.omit() %>% as.integer()
}

pmap(list(input, 
          lag(input, default = ".."), 
          lead(input, default = "..")), function(line,lag,lead){
            
            lines = c(lag,line,lead)
            stars = str_locate_all(line, "\\*")
            if(is_empty(stars[[1]])) {return(NULL)}
            pos = stars[[1]][,"start"]
            map(pos, function(p) {
              nums = get_nums(lines, p)
              if (length(nums)==2) {
                return(prod(nums))
              } else {
                return(NULL)
              }
            })
            
          }
) %>% unlist() %>% sum()

