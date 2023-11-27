library(tidyverse)

# Day 20: Jurassic Jigsaw ####
input20 <- read_lines("Day20/input20.txt")
# * - part 1 - *####
library(R6)

tile_names <- input20 %>% str_extract("[0-9]+") %>% na.omit() %>% as.character()
tiles <- list()
for (name in tile_names) {
  tiles[[name]] <- matrix(nrow = 10,ncol = 10, data = rep("",100))
}

input20clean <- input20[input20!=""]

for (row in input20clean) {
  if(str_detect(row, "Tile")) {
    irow <- 1
    tile_name <- str_extract(row, "[0-9]+")
  } else {
    tiles[[tile_name]][irow,] <- row %>% 
      # str_replace_all("\\.","0") %>% 
      # str_replace_all("#","1") %>%  
      str_split("") %>% sapply(as.character)
    irow <- irow + 1
  }
}

get_edges <- function(tile){
  up <- tile[1,] %>% 
    #as.character() %>% 
    str_c(collapse = "")
  right <- tile[,10] %>% 
    #as.character() %>% 
    str_c(collapse = "") 
  down <- tile[10,] %>% 
    #as.character() %>% 
    str_c(collapse = "") %>% 
    stringi::stri_reverse()
  left <- tile[,1] %>% 
    #as.character() %>% 
    str_c(collapse = "") %>% 
    stringi::stri_reverse()
  return(list(up=up, right=right, down=down, left=left))
}


# define frame object
# where to put the tiles
# used R6 just for fun

Frame <- R6Class(
  classname = "Frame",
  public =  list(
    
    matrix=NA,
    
    rotate = function() {
      self$matrix <- t(apply(self$matrix, 2, rev))
      invisible(self)
    },
    
    flip = function() {
      self$matrix <- self$matrix [c(dim(self$matrix)[1]:1),]
      invisible(self) 
    },
    
    initialize = function() {
      self$matrix <- matrix(rep(c("empty"),144), nrow=12)
    },
    
    print = function(...) {
      cat(paste0(self$matrix[1,1],"-",
                 self$matrix[1,dim(self$matrix)[2]],"\n",
                 self$matrix[dim(self$matrix)[1],1],"-",
                 self$matrix[dim(self$matrix)[1],dim(self$matrix)[2]]),"\n")
      invisible(self)
    }
    
  ))

the_frame <- Frame$new()

the_frame$matrix
the_frame

# Define the Tile object
# used R6 just for fun

Tile <- R6Class("Tile", 
                list(edges = list(),
                     
                     ID = NA_character_,
                     
                     matrix = NA_integer_,
                     
                     available = T,
                     position = NA_integer_,
                     
                     side = 1, # change when flip
                     
                     orientation=1, # change when rotate or flip
                     
                     allow_flip = T,
                     allow_rotation = T,
                     
                     possible_edges = NA_character_,
                     
                     rotate = function() {
                       
                       if(self$allow_rotation) {
                         self$orientation <- self$orientation + 1
                         if(self$orientation ==5) {self$orientation<-1}
                         
                         self$matrix <- t(apply(self$matrix, 2, rev))
                         
                         self$edges <- get_edges(self$matrix)
                       }
                       
                       invisible(self)
                     },
                     
                     flip = function() {
                       
                       if(self$allow_flip) {
                         self$side <- if_else(self$side==1,-1,1)
                         
                         self$orientation <- case_when(self$orientation==3 ~1,
                                                       self$orientation==1 ~3,
                                                       T~ self$orientation)
                         
                         self$matrix <- self$matrix [c(dim(self$matrix)[1]:1),]
                         
                         self$edges <- get_edges(self$matrix)
                       }
                       invisible(self)  
                       
                     },
                     
                     print = function(...) {
                       cat("\nTile:", self$ID,".",self$orientation,".",self$side, "\n",sep = "")
                       sapply(1:dim(self$matrix)[1], function(i) {
                         self$matrix[i,] %>% str_c(collapse = "")
                       }) %>% 
                         str_c(collapse = "\n") %>% 
                         cat()
                       invisible(self)
                     },
                     
                     list_possible_edges = function() {
                       self$possible_edges <- self$edges %>% unlist()
                       for (i in 1:3) {
                         self$possible_edges <- c(self$possible_edges, self$rotate()$edges %>% unlist())
                       }
                       self$flip()
                       self$possible_edges <- c(self$possible_edges, self$edges %>% unlist())
                       for (i in 1:3) {
                         self$possible_edges <- c(self$possible_edges, self$rotate()$edges %>% unlist())
                       }
                       
                       self$flip()
                       
                       self$possible_edges <- self$possible_edges %>% unique()
                       return(self$possible_edges)
                     },
                     
                     initialize = function(position=NA_integer_, matrix, ID) {
                       self$position <- position
                       self$matrix <- matrix
                       self$edges <- get_edges(self$matrix)
                       stopifnot(names(self$edges) == c("up","right","down","left"))
                       self$ID <- ID
                       self$possible_edges <- self$list_possible_edges()
                       
                     }
                )
)

Tiles <- imap(tiles, function(tile, id) {
  Tile$new(
    matrix = tile,
    ID = id
  )
})

testTile <- Tiles[[2]]

testTile
testTile$edges
testTile$list_possible_edges()

get_all_possible_edges <- function() {
  lapply(Tiles, function(x) {
    if (x$available) x$possible_edges
  }) %>%
    purrr::reduce(c)
}

all_possible_edges <- get_all_possible_edges()

#seems like the same edge is never present more than twice
all_possible_edges %>% table() %>% table()

#I will use this amount of matching edges
sqrt(length(Tiles))*(sqrt(length(Tiles))-1)*2

# given an edge, find the tile(s) that can be attached to it
find_tiles_by_edge <- function(Tiles, edge) {
  map(Tiles, function(tile) {
    if (tile$available & stringi::stri_reverse(edge) %in% tile$possible_edges) return(tile$ID) 
  }) %>% unlist()
}

find_tiles_by_edge(Tiles, "#..###...#")
find_tiles_by_edge(Tiles, testTile$edges[["up"]]) %>% setdiff(testTile$ID)
find_tiles_by_edge(Tiles, testTile$edges[["down"]]) %>% setdiff(testTile$ID)


# for each tile and orientation, I can count how many matches I can find
# all non edge tiles in the middle must have 4 matches
# boarder tiles must have 3 or more
# corner must have 2 or moe

# for a tile, count how many edges can be matched by available edges
count_matches <- function(tile) {
  map(tile$edges, function(edge) {
    find_tiles_by_edge(Tiles, edge) %>% setdiff(tile$ID)
  }) %>% unlist() %>% unique() %>% length()
} 

count_matches(testTile)
map_dbl(Tiles, count_matches)

is_central_tile <- function(tile) {
  count_matches(tile) == 4
}

is_central_tile(testTile)

# this must be >= 100! #or >=1 for test input
sum(sapply(Tiles, is_central_tile))

# find tiles with only 2,3,4 matching edges
count_of_matches <- sapply(Tiles, function(tile) {
  count_matches(tile)
})
count_of_matches

# and those are the corners, edge tiles, and middle tiles
corner_tiles <- Tiles[(count_of_matches==2)[count_of_matches==2] %>% names()]
edge_tiles <- Tiles[(count_of_matches==2)[count_of_matches==3] %>% names()]

find_directions <- function(tile) {
  map(tile$edges, function(x) {
    r <- find_tiles_by_edge(Tiles, x) %>% setdiff(tile$ID)
    if (length(r) >0) {r}
  }
  ) %>% unlist() %>% names()
}

find_directions(testTile)

add_position <- function(direction) {
  recode(direction, 
         "up"="-1,0",
         "down"="1,0",
         "left"="0,-1",
         "right"="0,1") %>% 
    str_split(",", simplify = T) %>% 
    as.integer()
}

add_position("right")

# pick the first corner and put it in the right orientation and position
corner1 <- corner_tiles[[1]]

corner1$position <- (lapply(find_directions(corner1), add_position) %>% 
                       purrr::reduce(`+`) ==1 )%>% 
  ifelse(1,dim(the_frame$matrix)[1])

corner1$available <- F
corner1$allow_flip <- F
corner1$allow_rotation <- F

the_frame$matrix[corner1$position[1],corner1$position[2]] <- corner1$ID

# Add tile to frame
# 1 pick a tile on the frame
# pick a direction where there are available tiles
# find the tile and put it in the right position on the frame
# update tile position, allow roate etc
add_tile <- function(tile) {
  
  dir <- find_directions(tile)[1]
  
  found <- tryCatch(
    error = function(condition) {
      stop("no tile found in this direction", call. = T)
    },
    expr = {Tiles[[tile$edges[dir] %>% find_tiles_by_edge(Tiles, .) %>% setdiff(tile$ID)]]}
  )
  try <- 0
  while(found$edges[dir] %>% stringi::stri_reverse() != tile$edges[dir]) {
    try <- try+1
    found$rotate()
    if (try==4) {found$flip()}
    if (try >9) {stop("deh!")}
  }
  found$rotate()$rotate()
  found$position <- tile$position+add_position(dir)
  found$available <- F
  the_frame$matrix[found$position[1],found$position[2]] <- found$ID
  found
}

last_tile <- corner1
while(sum(the_frame$matrix=="empty")>0) {
  last_tile <- add_tile(last_tile)
}

# OF FUCKING YEAH
as.integer(c(the_frame$matrix[1,1],
             the_frame$matrix[12,1],
             the_frame$matrix[1,12],
             the_frame$matrix[12,12])) %>% prod() %>% format(scientific=F)



# * - part 2 - *####
sea_monster <- read_lines("Day20/seamonster.txt") %>% str_replace_all(" ",".") 

sea_monsterL <- list(hidden = sea_monster,
                     regex = sea_monster %>% 
                       map_chr(str_replace_all,"[#]","[#]"),
                     found =sea_monster %>% 
                       map_chr(str_replace_all,"[#O]","O")
)

# crazy regex but I got it worked
#there must be an easy way
#must substitute A and B with str_locate indexes later
sea_monsterLx <- list(hidden = sea_monster,
                      regex = sea_monster %>% 
                        map_chr(str_replace_all,"[#]","[#]")%>% 
                        map_chr(function(x) {
                          pat <- str_replace_all(x,"(\\.+)","(\\1)")
                          str_c("^(.{A})",pat,"(.{B})$")}),
                      found =sea_monster %>% 
                        map_chr(str_replace_all,"[#O]","O") %>% 
                        map_chr(function(x) {
                          pat <- x
                          i <- 0
                          for (i in 1:str_count(pat,"\\.+")) {
                            pat <- str_replace(pat, "\\.+",str_c("\\\\",as.character(i+1)))
                            i<-i
                          }
                          str_c("\\1",pat,"\\",i+2)
                        })
)

cat(str_c(sea_monsterL$hidden,collapse = "\n"))

#create the image
image_matrix <- lapply(1:dim(the_frame$matrix)[1], function(i) {
  lapply(Tiles[the_frame$matrix[i,]], function(tile) {tile$matrix[2:9,2:9]}) %>% 
    purrr::reduce(cbind)
}) %>% purrr::reduce(rbind) 

rotate_matrix <- function(matrix) {
  t(apply(matrix, 2, rev))
}
flip_matrix <- function(matrix) {
  matrix [c(dim(matrix)[1]:1),]
}

print_image <- function(matrix) {
  matrix %>% 
    apply(1, str_c, collapse="")
}

image <- print_image(image_matrix)

# for testing
testimage_matrix <- read_lines("Day20/input20test2.txt") %>% 
  lapply(str_split,"",simplify=T) %>% 
  purrr::reduce(rbind)
testimage_matrix <- flip_matrix(testimage_matrix)
testimage_matrix <- rotate_matrix(testimage_matrix)
testimage <- print_image(testimage_matrix)
testimage2 <- print_image(cbind(testimage_matrix,testimage_matrix))


detect_monsters <- function(image=testimage2) {
  walk(2:(length(image)-1), function(line) {
    locate2all <- str_locate_all(image[line], sea_monsterL$regex[2])
    
    locate2 <- locate2all[[1]]
    
    #for (i in )
    if (length(locate2)==0) {return(F)} else {locate2 <- locate2[1,]}
    
    if (length(locate2)==2) {
      locate1 <- str_detect(str_sub(image[line-1],locate2[1],locate2[2]), sea_monsterL$regex[1])
      locate3 <- str_detect(str_sub(image[line+1],locate2[1],locate2[2]), sea_monsterL$regex[3])
      
      if (locate1&locate3) {
        # add replace # with 0 part
        image[line-1] <<- str_replace(image[line-1], 
                                      sea_monsterLx$regex[1] %>% 
                                        str_replace("A", as.character(locate2[1]-1)) %>% 
                                        str_replace("B", as.character(nchar(image[1])-locate2[2])),
                                      sea_monsterLx$found[1])
        image[line] <<- str_replace(image[line], 
                                    sea_monsterLx$regex[2] %>% 
                                      str_replace("A", as.character(locate2[1]-1)) %>% 
                                      str_replace("B", as.character(nchar(image[1])-locate2[2])),
                                    sea_monsterLx$found[2])
        image[line+1] <<- str_replace(image[line+1], 
                                      sea_monsterLx$regex[3] %>% 
                                        str_replace("A", as.character(locate2[1]-1)) %>% 
                                        str_replace("B", as.character(nchar(image[1])-locate2[2])),
                                      sea_monsterLx$found[3])
        
        #return(T)
      } else return(F)
      
    }else return(F)
  }) #%>% sum() 
  return(image)
}

detect_monsters(sea_monsterL$hidden)
detect_monsters(sea_monsterL$found)
detect_monsters(testimage)
detect_monsters(testimage2) #this must be fixed with str_locate_all
testimage2 <- detect_monsters(testimage2)
detect_monsters(testimage2)

sum(detect_monsters(testimage) %>% str_count("#"))

image_matrix <- rotate_matrix(image_matrix)
image_matrix <- flip_matrix(image_matrix)
image <-  print_image(image_matrix)

image <- detect_monsters(image)

sum(detect_monsters(image) %>% str_count("#"))

# need to add autorotate and auto iteretae detect monster a few times detect
# done interactively to get the solution

# this was CRAZY!
