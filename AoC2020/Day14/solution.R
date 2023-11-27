library(tidyverse)

input14 <- read_lines("Day14/input14.txt")
library(magrittr)
library(binaryLogic)

# * - part 1 - *####

code <- list()
iter <- 0

for (i in input14) {
  if (str_detect(i, "mask")) {
    iter <<- iter+1
    mask <- i %>% str_remove("mask = ")
    code[[iter]]<- list(mask=mask, mem=c(), val=c())
  }
  else if (str_detect(i, "mem")) {
    code[[iter]]$mem <- append(code[[iter]]$mem, str_replace(i, ".*\\[(.*)\\].*", "\\1"))
    code[[iter]]$val <- append(code[[iter]]$val, str_remove(i, ".*= "))
  }
}

memories <- lapply(code, function(x) x$mem) %>% unlist()

bin2dec <- function(x)
{
  b <- as.numeric(unlist(strsplit(x, "")))
  pow <- 2 ^ ((length(b) - 1):0)
  sum(pow[b == 1])
}

dec2bin <- function(x) {
  x %>% 
    intToBits() %>% 
    as.numeric() %>% 
    paste(collapse = "")}

get_masked_val <- function(mask, vals) {
  mask <-  mask %>% str_split("") %>% unlist()
  sapply(vals,USE.NAMES = F, function(val) {
    cval <- val %>% 
      as.numeric() %>%
      as.binary(n = 36, size = 8) %>%
      as.character()
    out <- map2_chr(cval,mask, function(val, mask) {
      if (mask=="X") {return(val %>% as.character())}
      else {return(mask)}
    })
    out %>% 
      paste(collapse = "") %>%
      bin2dec()
  })
}

coded <- lapply(code, function(x) {
  x$assigned <- get_masked_val(mask = x$mask,
                               vals = x$val)
  x
})

assigned <- sapply(coded, function(x) {x$assigned}) %>% unlist()
names(assigned) <- memories

unimem <- list()

#for (m in unique(memories)) {unimem[[m]] <- 0}

for (i in seq_along(assigned)) {unimem[[memories[[i]]]]<-assigned[[i]]}
# options(digits = 16)
unimem %>% unlist() %>% sum()

# * - part 2 - *####
mask_mem <- function(mask, mems) {
  mask <-  mask %>% str_split("") %>% unlist()
  
  sapply(mems,USE.NAMES = F, function(mem) {
    cmem <- mem %>% 
      as.numeric() %>%
      as.binary(n = 36, size = 8) %>%
      as.character()
    out <- map2_chr(cmem,mask, function(mem, mask) {
      if (mask=="X") {return(".")}
      else if (mask=="0") {return(mem %>% as.character())}
      else if (mask=="1") {return("1")}
    })
    out %>% 
      paste(collapse = "")
  }) 
}

masked <- lapply(code, function(x) {
  x$to_mem <- mask_mem(mask = x$mask,
                       mems = x$mem)
  x
})

expand_mem <- function(my_mem) {
  while(any(str_detect(my_mem, "\\."))) {
    zero <- str_replace(my_mem, "\\.","0")
    one <- str_replace(my_mem, "\\.","1")
    my_mem <- c(zero, one)
  }
  return(my_mem)
}

addressed <- lapply(masked, function(x) {
  x$addresses <- x$to_mem %>% lapply(expand_mem)
  x
})

unimem <- list()

for (i in seq_along(addressed)) {
  for (j in seq_along(addressed[[i]]$val)) {
    unimem[addressed[[i]]$addresses[[j]]] <- addressed[[i]]$val[j]
  }
}

unimem %>% as.numeric() %>% sum()
