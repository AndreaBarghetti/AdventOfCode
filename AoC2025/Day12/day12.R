library(tidyverse)

parse_input = function(file_path) {
  
  con <- file(file_path, "r")
  on.exit(close(con))
  
  # shapes
  shapes = list()
  regions = list()
  repeat {
    
    line <- readLines(con, n = 1, warn = FALSE) 
    
    if (length(line)==0){break}
    if (line == "") {next}
    
    if (str_detect(line, '^\\d+:')) {
      i = str_extract(line, '\\d+')
      next
    }
    if (str_detect(line,'[\\.|#]')) {
      shapes[[i]] = rbind( shapes[[i]], str_split(line, "", simplify = T))
      next
    }
    
    # boxes
    if (str_detect(line, 'x')) {
      nums = str_extract_all(line, '\\d+', simplify = T) %>% as.integer()
      regions = append(regions, list(list(dim = nums[1:2],
                                          presents = nums[-c(1:2)])))
      next
    }
    
  }
  list(shapes = shapes,
       regions = regions)
}

shapes_regions = parse_input("AoC2025/Day12/input.txt")

shapes = shapes_regions$shapes
regions = shapes_regions$regions

shapes_areas = map_int(shapes, ~sum(.x=="#"))
shapes_space = map_int(shapes, ~prod(dim(.x)))

# Part 1 ####
too_small = function(region) {
  area = prod(region$dim)
  min_req_area = sum(region$presents*shapes_areas)
  area < min_req_area
}
enough_space = function(region) {
  area = prod(region$dim)
  max_req_area = sum(region$presents*shapes_space)
  area >= max_req_area
}

map_chr(regions, function(r) {
  if (too_small(r)) {return('small')}
  if (enough_space(r)) {return('large')}
  return('???')
}) %>% table()

# no hard problems...
