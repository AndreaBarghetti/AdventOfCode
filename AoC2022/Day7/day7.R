library(tidyverse)

input <- readLines("Day7/input.txt")

is_cd <- function(input) {str_detect(input, "^\\$ cd*")}
is_ls <- function(input) {str_detect(input, "^\\$ ls*")}
is_file <- function(input) {str_detect(input, "^\\d+ .+$")}
is_dir <- function(input) {str_detect(input, "^dir.*")}

extract_cd <- function(input) {str_replace(input, "\\$ cd ","")}
extract_filename <- function(input) {str_replace(input, "^\\d+ ","")}
extract_filesize <- function(input) {str_replace(input, "^(\\d+) .+","\\1")%>% as.numeric()}

.cd <- function(path, d) {
  if (d == "..") {path = str_remove(path, "[^/]+/$")}
  else if (d == "/") {path = "/"}
  else {path = str_c(path, d, "/")}
  path
}

.file <- function(path, name, size) {
  path <- str_c(path, name)
  setNames(size, path)
}

scan_disk <- function(inputs) {

  path = "/"
  disk <- c()

  for (input in inputs) {
    # change path
    if (is_cd(input)) {
      d <- extract_cd(input)
      path <- .cd(path, d)
    }
    # add file to disk
    else if (is_file(input)) {
      name <- extract_filename(input)
      size <- extract_filesize(input)
      file <- .file(path, name, size)
      disk <- c(disk, file)
    }
    # add dir to disk
    else if (is_dir(input)) {
      name <- str_remove(input, "dir ") %>% paste0(., ".DIR")
      size <- 0
      file <- .file(path, name, size)
      disk <- c(disk, file)
    }
    else if (is_ls(input)) {
      next
    }
    else {
      stop("input not recognized: ", input)
    }
  }

  if (anyDuplicated(names(disk))) { warning("identical paths found!") }

  disk
}

get_recursive_size <- function(dirs, disk) {
  map_dbl(dirs, function(dir) {
    sum(disk[str_detect(names(disk), paste0("^",dir))])
  })
}

summarize_disk <- function(disk) {
  disk_df <- tibble(size=disk, path=names(disk)) %>%
    mutate(dir = str_remove(path, "[^/]+$")) %>%
    group_by(dir) %>%
    summarise(size=sum(size)) %>%
    mutate(rsize = get_recursive_size(dir,disk))
  disk_df
}

# part1 ####
solve_p1 <- function(input) {
  disk <- scan_disk(input)

  disk_df <- summarize_disk(disk)

  disk_df %>%
    filter(rsize <= 100000) %>%
    summarise(sum = sum(rsize)) %>%
    pull(sum)
}

solve_p1(input)

# part 2 ####
solve_p2 <- function(input,
                     total_disk_size = 70000000,
                     needed_disk_size = 30000000) {

  disk <- scan_disk(input)

  available = total_disk_size- sum(disk)
  missing = needed_disk_size-available

  disk_df <- summarize_disk(disk)

  disk_df %>%
    filter(rsize >= missing) %>%
    arrange(rsize) %>%
    pull(rsize) %>%
    pluck(1)
}
solve_p2(input)
