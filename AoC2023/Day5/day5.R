library(tidyverse)
input <- read_lines("AoC2023/Day5/input.txt")

parse_input = function(input){
  
  maps = list()
  
  for (line in input) {
    if (line =="") {next}
    if (str_detect(line,"seeds:")) {
      maps[["seeds"]] <- str_extract_all(line, "\\d+", simplify = T) %>% as.numeric()
      next
    }
    if (str_detect(line,":")) {
      map = line
      p = 0
      maps[[map]]<-list()
      next
    }
    p=p+1
    nums = str_extract_all(line,"\\d+", simplify = T) %>% as.numeric()
    nums = c(Start=nums[2], End= nums[2]+nums[3]-1, diff=nums[2]-nums[1])
    maps[[map]][[p]]<-nums
  }
  map(maps,reduce,rbind,deparse.level=0)
}

maps = parse_input(input)

# Part 1 ####
next_nums <- function(map, nums) {
  
  diffs = map[,"diff"]
  
  dests = map_dbl(nums, function(num) {
    
    rown = apply(map, 1, function(sed) {
      between(num, sed[1], sed[2])
    }) %>% which()
    if (length(rown)==0) {
      dest = num
    } else {
      dest = num - diffs[rown]
    }
    dest
  })
  dests
}

follow_map = function(maps) {
  nums=maps[[1]]
  for (lv in seq_along(maps)[-1]) {
    map = maps[[lv]]
    nums = next_nums(map, nums)
  }
  nums
}
follow_map(maps) %>% min()

# Part 2 ####
maps$seeds <- maps$seeds %>% matrix(ncol=2, byrow = T)
maps$seeds[,2] <- maps$seeds[,1]+maps$seeds[,2]-1
colnames(maps$seeds) <-  c("Start","End")

fill_map = function(map) {
  
  map <- map[order(map[,1]),]
  
  gaps <- list()
  gap_count <- 1
  
  end_point <- 0
  
  for (row in 1:nrow(map)) {
    start <- map[row, 1]
    end <- map[row, 2]
    
    if (start > end_point) {
      gaps[[gap_count]] <- c(end_point, start - 1, 0)
      gap_count <- gap_count + 1
    }
    end_point <- max(end_point, end + 1)
  }
  
  gaps = do.call(rbind, gaps)
  fmap = rbind(map,gaps)
  fmap <- fmap[order(fmap[,1]),]
  last = cbind(fmap[nrow(fmap),2]+1,Inf,0) %>% unname()
  rbind(fmap,last)
}

get_next_ranges <- function(map, ranges) {
  
  map = fill_map(map)
  cuts = sort(map[,1:2])
  
  next_ranges <- apply(ranges, 1, function(range) {
    betweens = cuts[between(cuts, range[1],range[2])]
    vertices = c(range[1],betweens,range[2])
    cut_ranges = matrix(c(vertices[-length(vertices)], (lead(vertices)-1)[-length(vertices)]), ncol = 2, byrow = F)
    nums = as.numeric(cut_ranges)
    next_ranges = next_nums(map,  nums) %>% matrix(ncol = 2)
    next_ranges
  }, simplify = F) %>% purrr::reduce(rbind, deparse.level = 0)
  colnames(next_ranges) <-  c("Start","End")
  next_ranges
}


follow_map = function(maps) {
  ranges=maps[[1]]
  for (lv in seq_along(maps)[-1]) {
    map = maps[[lv]]
    ranges = get_next_ranges(map, ranges)
  }
  ranges
}

res = follow_map(maps)

min(res)
