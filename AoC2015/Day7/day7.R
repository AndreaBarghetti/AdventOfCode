library(tidyverse)

pinput <- read_lines("Day7/input.txt")

read_instruction <- function(instruction) {

  fun <- str_extract(instruction, "AND|RSHIFT|LSHIFT|OR|NOT")
  if(is.na(fun)) {fun <- "EQUAL"}

  wires <- str_split(instruction, "->", simplify = T) %>%
    map(function(x) {
      str_extract_all(x, "[a-z0-9]+", simplify = T)
    })

  inputs <- wires[[1]]
  name <- wires[[2]]

  if(anyNA(c(name,inputs,fun))) {stop()}

  return(list(name=name,
              inputs=inputs,
              fun=fun))
}

# get wire value from env
get_wire_value <- function(name) {
  map_int(name %>% setNames(name) , function(name) {
    rlang::env_get_list(wires_env, nms=name) %>%
      unlist()
  })
}
#get_wire_value(wire_IDs)

#define operations
LSHIFT <- function(inputs) {
  output <- bitwShiftL(inputs[1], inputs[2])
  return(output)
}
RSHIFT <- function(inputs) {
  output <- bitwShiftR(inputs[1], inputs[2])
  return(output)
}
AND <- function(inputs) {
  output <- bitwAnd(inputs[1],inputs[2])
  return(output)
}
OR <- function(inputs) {
  output <- bitwOr(inputs[1],inputs[2])
  return(output)
}
EQUAL <- function(inputs) {
  output <- inputs
  return(output)
}
NOT <- function(inputs) {
  output <- bitwNot(inputs)
  return(output)
}



# check if instruction is ready to be evaluated
have_all_inputs <- function(instruction) {
  inputs <- instruction[["inputs"]]
  ids <- inputs[!inputs %>% str_detect("\\d+")]
  ready <- get_wire_value(ids) %>%
    map_lgl(is.na) %>% any()
  !ready
}
#map_lgl(instructions, have_all_inputs)


is_not_evaluated <- function(instruction) {
  value <- get_wire_value(name = instruction[["name"]])
  if (is.na(value)) {T} else {F}
}

#map_lgl(instructions, is_not_evaluated)


# evaluate instruction
evaluate_instruction <- function(instruction) {

  raw_inputs <- instruction[["inputs"]] %>% str_subset("\\d+") %>% as.integer()
  wire_inputs <- instruction[["inputs"]] %>% str_subset("\\d+",negate = T) %>%
    get_wire_value()

  inputs <- c(wire_inputs, raw_inputs) %>% unname()
  output <- do.call(what = instruction[["fun"]],
                    args = list(inputs=inputs))

  if(length(output) !=1) {print(instruction); stop("DEH!")}
  output
}
# evaluate_instruction(instructions[[55]])


#solve
instructions <- map(pinput, read_instruction)

wire_IDs <- instructions %>%
  map_chr(`[[`, "name") %>%
  unique()

wires_env <- as.list(vector("integer", length = length(wire_IDs)) %>%
                       setNames(wire_IDs) + NA_integer_) %>%
  rlang::as_environment()



while(is.na(get_wire_value("a"))) {
  walk(instructions, function(instruction) {

    wire_name <- instruction[["name"]]

    if (have_all_inputs(instruction) && is_not_evaluated(instruction)) {
      output <- evaluate_instruction(instruction)

      rlang::env_poke(env = wires_env,
                      nm = wire_name,
                      value = output)
    }
  })
  res <- get_wire_value("a")
}

wire_a <- get_wire_value("a")
wire_a

# PART 2
wires_env <- as.list(vector("integer", length = length(wire_IDs)) %>%
                       setNames(wire_IDs) + NA_integer_) %>%
  rlang::as_environment()

rlang::env_poke(env = wires_env,
                nm = "b",
                value = as.integer(wire_a))


while(is.na(get_wire_value("a"))) {
  walk(instructions, function(instruction) {

    wire_name <- instruction[["name"]]

    if (have_all_inputs(instruction) && is_not_evaluated(instruction)) {
      output <- evaluate_instruction(instruction)

      rlang::env_poke(env = wires_env,
                      nm = wire_name,
                      value = output)
    }
  })
  res <- get_wire_value("a")
}

wire_a <- get_wire_value("a")
wire_a
