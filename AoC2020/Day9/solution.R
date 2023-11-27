library(tidyverse)


input9 <- readLines("Day9/input9.txt")

# * - part 1 - *####
numbers <- input9 %>% as.numeric()

for (i in seq_along(numbers)) {
  
  if (i <= 25) {next()}
  
  if (numbers[i] %in% (numbers[(i-25):(i-1)] %>% 
                       combn(2) %>% 
                       colSums())) {next()}
  
  else {
    target <- numbers[i]
    print(target)}
}

# * - part 2 - *####
for (i in seq_along(numbers)) {
  for (j in 1:(length(numbers)-i)) {
    sum = sum(numbers[i:(j+i)])
    if (sum == target) {
      result <- numbers[i:(j+i)]
      print(sum(range(result)))
      stop("done!")}
  }
}
