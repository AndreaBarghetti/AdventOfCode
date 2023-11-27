library(tidyverse)

input13 <- read_lines("Day13/input13.txt")

# * - part 1 - *####
bus <- input13 %>% str_split(",") %>% unlist() %>% as.numeric()

wait <- (bus[-1] - (bus[1] %% bus[-1])) %>% min(na.rm = T)
getbus <- bus[-1][which(bus[-1] - (bus[1] %% bus[-1]) == wait)]

getbus*wait

# * - part 2 - *####
bus2 <- bus[-1]
delays <- 0:(length(bus2)-1)

bus3 <- bus2[!is.na(bus2)] %>% as.numeric()
delays2 <- delays[!is.na(bus2)]

lcm <- function(x, y) {
  # choose the greater number
  greater = max(x,y)
  i <- 1
  while(TRUE) {
    if((greater %% x == 0) && (greater %% y == 0)) {
      lcm = greater
      break
    }
    i <- i + 1
    greater = max(x,y) * i
  }
  return(lcm)
}

align <- function(x,y,delayx,delayy) {
  i <- x-delayx
  
  while(T) {
    if ((i+delayx)%%x == 0 & (i+delayy)%%y==0) {
      return(i)
      break
    }
    else {i <- i+x}
  }
}

# must covert to loop
i<-1
i<-i+1
delay <- align(bus3[1],bus3[i],delays2[1], delays2[i])
new <- lcm(bus3[1],bus3[i])
i<-i+1
delay <- align(new,bus3[i],new-delay,delays2[i])
new <- lcm(new,bus3[i])
new
