library(tidyverse)

input <- readLines("Day2/input.txt")

# part1 ####
rules <- c(RR = 1+3, RP = 2+6, RS = 3+0,
           PR = 1+0, PP = 2+3, PS = 3+6,
           SR = 1+6, SP = 2+0, SS = 3+3)

part1 <- function(input) {
  games <- input %>% str_replace_all(c("A"="R","B"="P","C"="S",
                                       "X"="R","Y"="P","Z"="S")) %>%
    str_remove_all(" ")

  points <- recode(games,!!!rules)
  sum(points)
}
part1(input)


# part2 ####
part2 <- function(input) {
  games <- input %>% str_replace_all(c("A"="R","B"="P","C"="S"))%>%
    str_remove_all(" ")

  strategy <- c(RX = "RS", RY = "RR", RZ = "RP",
                PX = "PR", PY = "PP", PZ = "PS",
                SX = "SP", SY = "SS", SZ = "SR")

  recode(games,!!!strategy) %>%
    recode(!!!rules) %>% sum()
}

part2(input)


# Visualization ####
getpoints1 <- function(input) {
  games <- input %>% str_replace_all(c("A"="R","B"="P","C"="S",
                                       "X"="R","Y"="P","Z"="S")) %>%
    str_remove_all(" ")

  points <- recode(games,!!!rules)
}
getpoints2 <- function(input) {
  games <- input %>% str_replace_all(c("A"="R","B"="P","C"="S"))%>%
    str_remove_all(" ")

  strategy <- c(RX = "RS", RY = "RR", RZ = "RP",
                PX = "PR", PY = "PP", PZ = "PS",
                SX = "SP", SY = "SS", SZ = "SR")

  recode(games,!!!strategy) %>%
    recode(!!!rules)
}

generate_random_game <- function(input) {
  randomhand <- sample(c("X","Y","Z"),length(input), replace = T)
  random_game <- str_sub(input,1,2) %>% str_c(randomhand)
  tibble(p1=getpoints1(random_game),
         p2=getpoints2(random_game))
}

random_results <- map_dfr(1:100, function(x) {
  generate_random_game(input) %>%
    mutate(X=x)
})


df <- tibble(p1=getpoints1(input), p2=getpoints2(input), X=0) %>%
  bind_rows(random_results) %>%
  group_by(X) %>%
  mutate(round=row_number(),
         cs1=cumsum(p1),
         cs2=cumsum(p2))

df2 <- df %>%
  select(round,cs1,cs2,X) %>%
  gather(part, points, c("cs1","cs2")) %>%
  mutate(color=ifelse(X==0,part,NA_character_),
         size=ifelse(X==0,.5,.1))

plot1 <- ggplot(df2) +
  geom_line(aes(x=round,y=points, col=color, group=interaction(X,part)),
            alpha=.5,
            size=df2$size,
            show.legend = F) +
  # facet_wrap(~part, scales="free") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        text = element_text(size=8)) +
  scale_x_log10() +
  scale_y_log10() +
  labs(y="Score", x=NULL)

plot2 <- df2 %>%
  group_by(part, round) %>%
  summarise(p = mean(points[X==0]>points[X!=0])) %>%
  ggplot(aes(x=round,y=p, col=part)) +
  geom_line(show.legend = F) +
  # theme_void() +
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        text = element_text(size=8)) +
  scale_x_log10() +
  labs(y="Cheating detection",x="Round")


image <- ggpubr::ggarrange(plot1,plot2, ncol = 1, align = "v")

ggsave(plot = image,
       dpi = 300,
       filename = "plot.png",
       device = "png",
       path = "Day2/",
       width = 8,
       height = 8, units = "cm")
