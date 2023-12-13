library(tidyverse)
input <- read_lines("AoC2023/Day12/input.txt")

parse_input <- function(input) {
  sequences = str_extract(input, "^\\S+")
  counts = str_extract_all(input, "\\d+") %>% 
    map(as.integer)
  map2(sequences, 
       counts,
       function(sequence, counts) {
         list(sequence=sequence,
              counts=counts)
       })
}

springs = parse_input(input)

# Part 1 ####
get_all_seqs <- function(spring) {
  
  unknown = str_count(spring$sequence,"\\?")
  damaged = str_count(spring$sequence,"#")
  total_damaged = sum(spring$counts)
  missing_damaged = total_damaged-damaged
  unknown_loc = str_locate_all(spring$sequence,"\\?")[[1]]
  
  possible_seqs = combn(unknown, missing_damaged, FUN = function(x) {
    seq <- stringi::stri_sub_replace_all(spring$sequence,matrix(unknown_loc[x,], ncol=2),replacement = "#")
    seq = str_replace_all(seq,"\\?","\\.")
    seq
  })
  possible_seqs
}

check_counts = function(spring) {
  possible_seqs = get_all_seqs(spring)
  str_extract_all(possible_seqs,"#+") %>% 
    map(nchar) %>% 
    map_lgl(identical, spring$counts)
}

count_seqs <- function(springs){
  map_int(springs, function(spring) {
    check_counts(spring) %>% sum() 
  })
}

sum(count_seqs(springs))

# Part 2 ####
unfold_springs <- function(springs) {
  
  map(springs, function(spring) {
    
    spring$sequence <- map_chr(1:5, ~{spring$sequence}) %>% 
      str_c(collapse = "?")
    spring$counts <- map(1:5, ~{spring$counts}) %>% 
      unlist()
    
    spring
  })
  
}

springs2 <- unfold_springs(springs)

# I wrote this part by looking at the solution from hyper-neutrino
# https://github.com/hyper-neutrino/advent-of-code/tree/main/2023

count_seqs_dp <- function(sequence,counts, cache) {
  
  key <- paste(sequence, toString(counts))
  
  if (exists(key, envir = cache)) {
    return(get(key, envir = cache))
  }
  
  if (sequence == "") {
    return(ifelse(length(counts) == 0, 1, 0))
  }
  
  if (length(counts) == 0) {
    return(ifelse(str_detect(sequence,"#"), 0, 1))
  }
  
  result <- 0
  
  if (str_sub(sequence, 1, 1) %in% c(".", "?")) {
    result <- result + count_seqs_dp(str_sub(sequence, 2, -1), counts, cache)
  }
  
  if (str_sub(sequence, 1, 1) %in% c("#", "?")) {
    if (counts[1] <= nchar(sequence) && str_detect(str_sub(sequence, 1, counts[1]),"\\.", negate = T) &&
        (counts[1] == nchar(sequence) || str_sub(sequence, counts[1] + 1, counts[1] + 1) != "#")) {
      result <- result + count_seqs_dp(str_sub(sequence, counts[1] + 2, -1), counts[-1], cache)
    }
  }
  
  cache[[key]] <- result
  return(result)
}

count_seqs2 <- function(springs) {
  
  cache <- rlang::env()
  map_dbl(springs, function(spring) {
    count_seqs_dp(spring$sequence, spring$counts, cache)
  })
  
}

result = sum(count_seqs2(springs2))

count_seqs2(springs)

format(result, scientific = F)
