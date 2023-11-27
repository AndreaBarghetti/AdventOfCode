parse_rocks <- function(rocks_file) {

  rocks <- read_lines(rocks_file)

  rocks <- split(rocks[rocks!=""],cumsum(rocks=="")[rocks!=""])

  rocks <- map(rocks, function(rock) {
    rock %>%
      map(str_split,pattern="", simplify=T) %>%
      purrr::reduce(rbind)
  }) %>%
    map(~ifelse(. ==".",0,1))

  rocks <- structure(rocks, I = length(rocks), class = c("cyclic_vector", class(rocks)))

  rocks
}

parse_winds <- function(input_file) {
  winds <- read_lines(input_file) %>%
    str_replace_all(c(">"="1","<"="-1")) %>%
    str_extract_all("[-]*1", simplify = T) %>%
    as.integer()

  winds <- structure(winds, I = length(winds), class = c("cyclic_vector", class(winds)))

}

recycle_index <- function(x, i){
  N <- length(x)
  x[[((i)-1)%%N+1]]
}

init_tower <-  function() {
  tower <- matrix(2, nrow = 1, ncol = 9)
  tower
}

expand_tower <- function(tower, n) {
  rbind(matrix(rep(c(2,0,0,0,0,0,0,0,2),each=n),nrow = n),
        tower)
}

place_rock <- function(tower, rock) {

  tower <- expand_tower(tower, 3)

  tower <- expand_tower(tower, nrow(rock))

  tower[1:nrow(rock),4:(3+ncol(rock))] <- rock

  tower
}

move_rock <- function(tower, wind) {

  if (wind==1) {
    test <- (tower==1) %>%
      apply(1,lag,default=F) %>%
      t() + ifelse(tower==1,0,tower)
  } else {
    test <- (tower==1) %>%
      apply(1,lead,default=F) %>%
      t() + ifelse(tower==1,0,tower)
  }

  if (any(test==3)) {tower} else {test}

}

trim_tower <- function(tower) {
  repeat{
    if (sum(tower[1,2:8])==0) {
      tower <- tower[-1,]
      if (is.null(nrow(tower))) {tower <- matrix(tower, nrow = 1)}
    } else {break}
  }

  tower
}

lower_rock <- function(tower) {
  test <- (tower==1) %>%
    apply(2,lag,default=F) + ifelse(tower==1,0,tower)

  if (any(test==3)) {
    attributes(tower)$next_rock <- T
    return(tower)
  } else {return(test)}
}

fall_rock <- function(tower) {

  while(is.null(attributes(tower)$next_rock)) {
    tower <- move_rock(tower,
                       wind = recycle_index(winds,wind_i))
    tower <- lower_rock(tower)
    wind_i <<- wind_i+1
  }

  nrow(tower)+attr(tower, "trimmed")-1

  attr(tower,"next_rock") <- NULL
  tower[tower==1]<-2
  tower
}

fall_n_rocks <- function(tower, N, max=50) {

  trimmed <- 0

  while(rock_count<N) {
    rock_count <<- rock_count+1
    tower <- place_rock(tower = tower,
                        rock = recycle_index(rocks,rock_count))
    tower <- fall_rock(tower)
    tower <- trim_tower(tower)

    if(nrow(tower) > max) {
      trimmed = trimmed + nrow(tower)-max
      tower <- tower[1:max,]
    }

  }

  attr(tower,"trimmed") <- trimmed

  return(tower)
}

fall_n_rocks2 <- function(tower, N, max=50) {

  trimmed <- 0

  while(rock_count<N) {
    rock_count <<- rock_count+1
    tower <- place_rock(tower = tower,
                        rock = recycle_index(rocks,rock_count))
    tower <- fall_rock(tower)
    tower <- trim_tower(tower)

    if(nrow(tower) > max) {
      trimmed = trimmed + nrow(tower)-max
      tower <- tower[1:max,]
    }

    heights <<- c(heights,
                  trimmed+nrow(tower)-1)
  }

  attr(tower,"trimmed") <- trimmed

  return(tower)
}


# check_repeat_end <- function(x, min_len) {
#   for (i in min_len:length(x)) {
#     word <- paste(x,collapse = "")
#     patt <- str_sub(word, -min_len,-1)
#     searchable <- str_sub(word, 1, -min_len-1)
#     located <- str_locate(searchable, patt)
#     if (!is.na(located[1])) {
#       start <- located[1]
#       period <- length(x)-min_len-start+1
#       return(c(start=start,period=period))
#     }
#   }
#   return(NA)
# }
#
# find_period <- function(x, min_len) {
#   for (i in (2*min_len):length(x)) {
#     test <- x[1:i]
#     res <- check_repeat_end(test,min_len)
#     if(!is.na(res[1])) {break}
#   }
#   res
# }

find_period2 <- function(x, min_len) {
  word <- paste(x,collapse = "")
  patt <- str_sub(word, -min_len,-1)
  searchable <- str_sub(word, 1, -min_len-1)
  located_first <- str_locate(searchable, patt)
  searchable <- str_sub(word, located_first[2]+1,-1)
  located_second <- str_locate(searchable, patt)+located_first[2]
  return(c(start=located_first[1],
           period=located_second[1]-located_first[1]))
}

# test
# x <- c(1, 2, 3, 4, rep(c(5,6,4,3,2,7), 10))
# find_period2(x,4)
