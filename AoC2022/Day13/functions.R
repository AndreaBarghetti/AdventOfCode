unpack_elements <- function(string) {

  if(string=="[]") {return(vector(mode = "character", length = 0))}

  split <- string %>% str_split("", sim=T)
  grps <- list()
  lv=0
  elem <- 1
  grps[[elem]]<-vector(mode = "character", length = 0)

  for (let in split[-c(1,length(split))]) {
    if (let =="[") {lv=lv+1}
    if (let =="]") {lv=lv-1}
    if (let=="," & lv == 0) {
      elem = elem+1
      grps[[elem]]<-vector(mode = "character", length = 0)
      next
    }
    {grps[[elem]] <- c( grps[[elem]],let)}
  }
  grps %>% map_chr(str_c, collapse="")
}

# unpack_elements("[4,5,[6],[7,[8,[9],[]]],[]]")
# unpack_elements("[]")

islist <- function(string) {
  if (length(string) ==0) {return(F)}
  str_detect(string, "^\\[")
}

# islist(unpack_elements("[]"))
# islist("[]")

# unpack_elements("[[4,5],6,7,[],[[[5,[]]]],[1]]")
pack_element <- function(string) {
  if (length(string) ==0) {return("[]")}
  str_c("[",string,"]")
}

# pack_element("5")
# pack_element(unpack_elements("[5]"))


compare_LR <- function(L,R) {

  max_length <- max(length(L),length(R))

  if (max_length > 0 ) {
    for (i in 1:max_length) {

      # if one list runs out of elements
      if (i> length(L)) { return(T) }

      if (i> length(R)) { return(F) }

      # if both are integer
      if ((!islist(L[i])) & (!islist(R[i]))) {
        if (as.integer(L[i]) < as.integer(R[i])) {return(T)}
        if (as.integer(L[i]) > as.integer(R[i])) {return(F)}
      }

      # if comparing list to int, convert int to list
      if (islist(L[i]) & (!islist(R[i]))) {
        R[i] <- pack_element(R[i])
      }
      if (islist(R[i]) & (!islist(L[i]))) {
        L[i] <- pack_element(L[i])
      }

      # if comparing lists:
      if (islist(L[i]) & islist(R[i])) {

        # unpack list
        l <- unpack_elements(L[i])
        r <- unpack_elements(R[i])

        # return(compare_LR(l, r))
        res <- compare_LR(l, r)
        if (is.logical(res)){
          return(res)
        }
      }
    }
  }
}
