library(tidyverse)


# Day 4 ####
input4 <- readLines("Day4/input4.txt") %>% 
  lapply(function(x) {
    if (x=="") "NEXTPASSPT"
    else x
  }) %>% paste(collapse = " ") %>%
  str_split("NEXTPASSPT") %>%
  unlist() %>% 
  str_remove_all("^ | $")


# * - part 1 - *####
fields <- c("byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:", "cid:")

names(fields) <- fields

scan <- lapply(input4, function(x) {
  passport <- x %>%
    str_detect(pattern = fields)
  names(passport) <- fields
  return(passport %>% 
           as.list() %>% 
           as_tibble())
})

scan_df <- scan %>% purrr::reduce(bind_rows)

passidx <- rowSums(scan_df %>% select(-`cid:`))==7
sum(passidx)

# * - part 2 - *####
scan <- lapply(input4, function(x) {
  passport <- x  %>% str_split(" ") %>% unlist()
  names(passport) <- passport %>% 
    str_remove(":.*")
  return(passport %>% 
           as.list() %>% 
           as_tibble())
})

scan_df <- scan %>% purrr::reduce(bind_rows)

checkpass <- scan_df %>%
  mutate_all(.funs = function(.) str_remove(., pattern = ".*:")) %>%
  mutate(byr = byr %>% as.numeric() %>% between(1920,2002),
         iyr = iyr %>% as.numeric() %>% between(2010,2020),
         eyr = eyr %>% as.numeric() %>% between(2020,2030),
         hgt = case_when(str_detect(hgt, "cm") ~ hgt %>% str_remove("cm") %>% as.numeric() %>% between(150,193),
                         str_detect(hgt, "in") ~ hgt %>% str_remove("in") %>% as.numeric() %>% between(59,76),
                         is.na(hgt) ~ F,
                         T~F),
         hcl = str_detect(hcl, pattern = "#[a-f0-9]{6}"),
         ecl = ecl %in% c("amb","blu","brn","gry", "grn","hzl","oth"),
         pid = pid %>% str_detect("^[0-9]{9}$"))


passidx2 <- checkpass %>%
  select(-cid) %>%
  rowSums(na.rm=T) == 7

sum(passidx2)
#
