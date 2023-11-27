library(tidyverse)

input <- readLines("Day5/input.txt")

parse_input <- function(input) {

  sep_line = (which(input==""))
  input_stacks <- input[1:(sep_line-2)]
  input_procedures <- input[-c(1:sep_line)]

  stacks <- read_stacks(input_stacks)
  procedures <- read_procedures(input_procedures)

  list(stacks=stacks,
       procedures=procedures)

}

read_stacks <- function(stacks) {
  ls <- map(stacks, function(stack) {
    stack %>%
      str_extract_all("\\[.\\]|    |   ") %>%
      unlist() %>%
      str_replace_all(" +", "") %>%
      str_replace_all("[\\[\\]]", "")
  })

  len = length(ls[[1]])
  wid = length(ls)
  ls %>%
    purrr::reduce(rbind) %>%
    apply(2,function(x) {str_c(x[wid:1], collapse="")}) %>%
    setNames(1:len)
}

read_procedures <- function(procedures) {
  map(procedures, function(procedure) {
    procedure %>%
      str_extract_all("\\d+") %>%
      unlist() %>%
      as.integer() %>%
      setNames(c("N","from","to"))
  })
}

operate_procedure <- function(procedure, stacks, model=c("9000","9001")) {

  model=match.arg(model)
  N <- procedure["N"]
  from_stack <- stacks[procedure["from"]]
  to_stack <- stacks[procedure["to"]]

  crates <- str_sub(from_stack, -N,-1)
  if (model == "9000") {crates = crates %>% stringi::stri_reverse()}
  stacks[procedure["to"]] <- str_c(to_stack,crates)
  stacks[procedure["from"]] <- str_sub(from_stack, 1,-(N+1))
  return(stacks)
}

operate_procedures <- function(parsed_input, model) {
  stacks <- parsed_input[["stacks"]]

  for (procedure in parsed_input[["procedures"]]) {
    stacks <- operate_procedure(procedure, stacks, model = model)
  }
  stacks
}

# Part 1 ####
parsed_input <- parse_input(input)

operate_procedures(parsed_input, model="9000") %>%
  str_sub(-1,-1) %>% str_c(collapse="")

# Part 2 ####
operate_procedures(parsed_input, model="9001") %>%
  str_sub(-1,-1) %>% str_c(collapse="")


# Visualization ####
library(animation)

plot_stacks <- function(stacks) {

  split_stacks <- stacks %>%
    map(str_split,"", simplify=T)

  stacks_df <- imap_dfr(split_stacks, function(split_stack, name) {

    if (length(split_stack)==0) {return(tibble())}
    split_stack %>%
      `colnames<-`(.,1:length(.)) %>%
      as_tibble() %>%
      pivot_longer(values_to = "letter",
                   names_to = "y", cols = all_of(names(.))) %>%
      mutate(x=as.integer(name),
             y=as.integer(y),
             letter=factor(letter, levels=LETTERS))
  })

  stacks_df %>%
    ggplot(aes(xmin=x-.90,
               xmax=x,
               ymin=y-1,
               ymax=y,
               fill=letter)) +
    geom_rect(col="black", show.legend = F) +
    theme_void() +
    scale_fill_discrete(drop=FALSE) +
    coord_cartesian(xlim=c(0,9), ylim=c(0,50))
}

save_animation <- function(parsed_input) {

  stacks <- parsed_input[["stacks"]]

  saveGIF({
    for (procedure in parsed_input[["procedures"]]) {
      stacks <- operate_procedure(procedure, stacks, model = "9001")
      print(plot_stacks(stacks))
    }
  },
  loop=T,
  interval=0.25,
  ani.width = 250,
  ani.height = 400,
  movie.name = "animation.gif",
  outdir = "./Day5/"
  )
}

save_animation(parsed_input)
