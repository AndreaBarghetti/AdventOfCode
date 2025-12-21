library(tidyverse)
input <- read_lines("AoC2024/Day19/input.txt")

parse = function(input) {
  patterns = input[1] %>% str_split(", ") %>% unlist()
  designs = input[-c(1:2)] %>% unlist()
  return(list(patterns=patterns,designs=designs))
}

patterns = parse(input)$patterns
designs = parse(input)$designs

# Part 1 ####
match_design = function(design, patterns) {
  if (any(patterns==design)) {return(T)}

  for (p in patterns) {
    if (str_starts(design,p)) {
      cut_design = str_remove(design,p) 
      if (match_design(cut_design, patterns)) {return(T)}
    }
  }
  return(FALSE)
}

sum(map_lgl(designs, match_design, patterns))

# Part 2 ####

# all attempts are too slow for real input

library(igraph)

extend_pattern = function(pattern, design, patterns) {
    pts = str_c(pattern,patterns,"")
    keep = str_starts(design, pts)
    pts[keep]
}

count_paths = function(design, patterns) {
  design=str_c(".",design,collapse = "")
  start="."
  
  queue = collections::queue()
  queue$push(start)
  
  g = igraph::make_empty_graph(directed = T)
  g = igraph::add_vertices(g, 2, name=c(".",design))
  
  while( queue$size()>0) {
    
    p = queue$pop()
    nn = extend_pattern(p, design, patterns)
    g = igraph::add_vertices(g,length(nn), name = nn)
    
    for (n in nn) {
      g = igraph::add_edges(g, c(p, n))
      queue$push(n)
    }

  }
  
  igraph::all_simple_paths(g,".",design) %>% length()
  
}

map_dbl(designs, count_paths, patterns) %>% sum()


### old
count_combs = function(design, patterns) {
  
  dict = collections::dict()
  dict$set("",1)

  pts = extend_pattern("",design,patterns) 
  
  test = extend_pattern(pts,design,patterns) %>% table()
  test = extend_pattern(names(test),design,patterns) %>% table()
  
  dict$keys() %>% unlist()
  
}
  



count_combs = function(design, patterns) {
  
  queue = collections::deque()
  for (p in patterns) {
    queue$push(p)
  }
  counter=0
  while (queue$size()>0) {

    str = queue$pop()
    if(str==design) {counter=counter+1; next}
    if (str_starts(design,str,negate = T)) {next}
    
    ns = str_c(str,patterns, "")
    pass=str_starts(design,ns)
    if (!any(pass)) {next}
    ns=ns[pass]
    for (s in ns) {
      queue$push(s)
    }  
  }

  counter
}

map_dbl(,count_combs, patterns)

counts = map_dbl(designs, count_combs, patterns)

sum(counts)


####
counter = rlang::env()
walk(designs,function(d) {
  rlang::env_poke(env = counter, nm = d, value = 0)
})

add_combo = function(design) {
  rlang::env_poke(counter, design, rlang::env_get(counter, design) + 1)
}

count_combs = function(design, l, patterns) {
  if (any(patterns==design)) {
    if(nchar(design)==l){add_combo(design)}
    return(T)
  }
  for (p in patterns) {
    if (str_starts(design,p)) {
      cut_design = str_remove(design,p) 
      if (count_combs(cut_design, l, patterns)) {
        if(nchar(design)==l){add_combo(design)}
        return(T)
      }
    }
  }
  return(FALSE)
}

dlen = nchar(designs)
map2_lgl(designs, dlen, function(design, l ){count_combs(design,l,patterns)} )

rlang::as_list(counter)
