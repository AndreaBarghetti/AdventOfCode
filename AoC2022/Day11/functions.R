# parse monkey 6 raw text lines into list
read_monkey <- function(raw_monkey) {
  name <- raw_monkey[1] %>% str_extract("\\d+")
  items <- raw_monkey[2] %>% str_extract_all("\\d+") %>% unlist() %>% as.integer()
  operation <- raw_monkey[3] %>% str_extract("new =.*") %>% str_c(";new")
  test <- raw_monkey[4] %>% str_extract("\\d+") %>% as.numeric()
  test_T <- raw_monkey[5] %>% str_extract("\\d+")
  test_F <- raw_monkey[6] %>% str_extract("\\d+")
  list(name=name,
       items=items,
       operation=operation,
       test=test,
       test_T=test_T,
       test_F=test_F,
       counter = 0)
}

# evaluate monkey's test based on worry level
eval_test <- function(worry_lv, test) {
  worry_lv%%test==0
}

# evaluate monkey's operation based on worry level
eval_operation <- function(operation, old) {
  eval(parse(text = operation))
}

# calm down to reduce worry level
calm_down <- function(worry_lv) {
  floor(worry_lv/3)
}

# throw first item in the items
# and change its value to new worry_lv
# and increse counter by 1
throw_item <- function(from, to, worry_lv) {
  M1 <- rlang::env_get(env = .monkeys_env, nm = from)
  M2 <- rlang::env_get(env = .monkeys_env, nm = to)
  M2$items <- c(M2$items, worry_lv)
  M1$items <- M1$items[-1]
  M1$counter <- M1$counter + 1
  rlang::env_poke(env = .monkeys_env, nm = from, value = M1)
  rlang::env_poke(env = .monkeys_env, nm = to, value = M2)
}

# A turn:
# repeat for each item
# 1 get item and increase worry level
# 2 calm down
# 3 evaluate test
# 4 choose who to throw to
# 5 throw
# 6 increase inspection counter
play_turn <- function(monkey_name) {
  Monkey <- rlang::env_get(env = .monkeys_env, nm = monkey_name)
  M1 <- Monkey$name

  for (item in Monkey$items) {
    # 1-2 get item and increase worry level and calm down
    worry_lv <- eval_operation(Monkey$operation, item) %>%
      calm_down()
    # 3 evaluate test
    test_res <- eval_test(worry_lv, Monkey$test)
    # 4 choose who to throw to
    M2 <- ifelse(test_res, Monkey$test_T,Monkey$test_F)
    # 5-6 throw and increase inspection counter
    throw_item(from = M1, to = M2, worry_lv = worry_lv)
  }
}

# A round:
# repeat 1 turn for each monkey
# in the round order
play_round <- function(round_order) {
  for (monkey in round_order) {
    play_turn(monkey)
  }
}

# run N rounds
play_rounds <- function(N, round_order) {
  for (i in 1:N) {
    play_round(round_order)
  }
}

# get result
get_result <- function() {
  rlang::env_get_list(env = .monkeys_env, nms = round_order) %>%
    map_dbl(`[[`, "counter") %>%
    sort(decreasing = T) %>%
    `[`(1:2) %>%
    prod()
}

