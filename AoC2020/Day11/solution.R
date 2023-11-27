library(tidyverse)

# Day 11 ####
input11 <- readLines("Day11/input11.txt")

# * - part 1 - *####
floors <- sapply(input11, USE.NAMES = F, function(string) {
  str_split(string, "") %>%
    unlist() != "L"
}) %>% t()


occupied_chairs <- matrix(F, nrow = nrow(floors),ncol=ncol(floors))

positions <-  matrix(seq_along(floors), nrow=nrow(floors))

get_neighbour_matrix <- function(matrix, position) {
  nrow <- nrow(matrix)
  ncol <-  ncol(matrix)
  pos <- which(position==positions, 
               arr.ind=TRUE)
  row_start <- max(1, pos[1]-1)
  row_end <- min(pos[1]+1, nrow)
  col_start <- max(1, pos[2]-1)
  col_end <- min(pos[2]+1, ncol)
  
  return(matrix[row_start:row_end,col_start:col_end])
}

take_seat <- function(matrix, position) {
  neighbours <- sum(get_neighbour_matrix(matrix, position))
  if (neighbours == 0) return(TRUE)
  else if(neighbours > 4) return(FALSE)
  else return(matrix[position])
}

stable <- FALSE
i<-0
while(!stable) {
  #seating
  seating <- sapply(positions, function(position) {
    take_seat(occupied_chairs, position)
  }) %>% matrix(nrow=nrow(occupied_chairs))
  #empty_floor
  seating[floors] <- FALSE
  stable <- all(seating == occupied_chairs)
  occupied_chairs <- seating
  i <- i+1
  print(i)
}

sum(occupied_chairs)

to_print <- occupied_chairs %>%
  as.numeric() %>%
  recode("1"="#","0"="L") %>%
  matrix(nrow=nrow(occupied_chairs))
to_print[floors] <- "."
apply(to_print, 1, paste, collapse="") %>%
  paste(collapse="\n") %>% 
  cat()

# * - part 2 - *####

#add extra floor
input11 <- readLines("Day11/input11.txt")

floors <- sapply(input11, USE.NAMES = F, function(string) {
  str_split(string, "") %>%
    unlist() == "."
}) %>% t()

positions <-  matrix(seq_along(floors), nrow=nrow(floors))

chairs <- matrix("L", nrow = nrow(floors),ncol=ncol(floors))
chairs[floors] <- "_"

chair_positions <- positions[chairs=="L"]

stable <- FALSE
i<-0

#
take_seat2 <- function(matrix, position) {
  nrow <- nrow(matrix)
  ncol <-  ncol(matrix)
  pos <- which(position==positions, 
               arr.ind=TRUE)
  
  
  #N
  see <- "_"
  look <- pos
  while(!see %in% c("#","L")) {
    look[1] <- look[1]-1
    if (look[1]<1) {
      see<-"_"
      break()}
    else if (matrix[look] %in% c("#","L")) {
      see <- matrix[look]
      break()
    }
  }
  N <- see
  #S
  see <- "_"
  look <- pos
  while(!see %in% c("#","L")) {
    look[1] <- look[1]+1
    if (look[1]>nrow) {
      see<-"_"
      break()}
    else if (matrix[look] %in% c("#","L")) {
      see <- matrix[look]
      break()
    }
  }
  S <- see
  #W
  see <- "_"
  look <- pos
  while(!see %in% c("#","L")) {
    look[2] <- look[2]-1
    if (look[2]< 1) {
      see<-"_"
      break()}
    else if (matrix[look] %in% c("#","L")) {
      see <- matrix[look]
      break()
    }
  }
  W <- see
  # E
  see <- "_"
  look <- pos
  while(!see %in% c("#","L")) {
    look[2] <- look[2]+1
    if (look[2]> ncol) {
      see<-"_"
      break()}
    else if (matrix[look] %in% c("#","L")) {
      see <- matrix[look]
      break()
    }
  }
  E <- see
  #NW
  see <- "_"
  look <- pos
  while(!see %in% c("#","L")) {
    look <- look-1
    if (look[1]<1 | look[2]<1) {
      see<-"_"
      break()}
    else if (matrix[look] %in% c("#","L")) {
      see <- matrix[look]
      break()
    }
  }
  NW <- see
  #NE
  see <- "_"
  look <- pos
  while(!see %in% c("#","L")) {
    look[1] <- look[1]-1
    look[2] <- look[2]+1
    if (look[1]<1 | look[2]>ncol) {
      see<-"_"
      break()}
    else if (matrix[look] %in% c("#","L")) {
      see <- matrix[look]
      break()
    }
  }
  NE <- see
  
  #SE
  see <- "_"
  look <- pos
  while(!see %in% c("#","L")) {
    look <- look+1
    if (look[1]>nrow | look[2]>ncol) {
      see<-"_"
      break()}
    else if (matrix[look] %in% c("#","L")) {
      see <- matrix[look]
      break()
    }
  }
  SE <- see
  
  #SW
  see <- "_"
  look <- pos
  while(!see %in% c("#","L")) {
    look[1] <- look[1] +1
    look[2] <- look[2] -1
    if (look[1]>nrow | look[2]<1) {
      see<-"_"
      break()}
    else if (matrix[look] %in% c("#","L")) {
      see <- matrix[look]
      break()
    }
  }
  SW <- see
  
  visible <- c(N,NE,E,SE,S,SW,W,NW) == "#"
  
  if (sum(visible)==0) return("#")
  else if (sum(visible)>=5) return("L")
  else return(matrix[position])
}

while(!stable) {
  #seating
  seating <- sapply(chair_positions, function(position) {
    take_seat2(chairs, position)
  }) 
  
  chairs2 <- chairs
  chairs2[chair_positions] <- seating
  
  stable <- all(chairs2 == chairs)
  chairs <- chairs2
  i <- i+1
  if(F) {cat(i)
    
    cat("\n\n")
    
    chairs %>%
      apply(1, paste, collapse="") %>%
      paste(collapse="\n") %>% 
      cat()
    
    cat("\n\n")}
  
  if(F) {cat(paste(sum(chairs=="#"),"\n"))}
}

sum(chairs=="#")
