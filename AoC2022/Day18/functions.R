parse_input <- function(file) {

  input <- read_lines(file) %>%
    map_dfr(~str_split(.x, pattern = ",", simplify=T) %>%
          as.integer() %>% `+`(1) %>%
          setNames(c("x","y","z")))

}

.spread_water <- function(water, axis) {
  water %>%
    mutate({{axis}}:=map({{axis}},`+`, (-1:1))) %>%
    unnest({{axis}})
}

spread_water <- function(water, rock) {
  bind_rows(.spread_water(water,x),
            .spread_water(water,y),
            .spread_water(water,z)) %>%
    unique() %>%
    filter(between(x, 0,20),
           between(y, 0,20),
           between(z, 0,20)) %>%
    anti_join(rock, by=c("x","y","z"))
}

spread_water_all <- function(water, rock) {
  nwater<- nrow(water)
  repeat {
    water <- spread_water(water, input)
    if (nrow(water) == nwater) {break}
    nwater <- nrow(water)
  }
  water
}


get_surface_area <- function(coord_df) {

  distances <- coord_df %>%
    as.matrix() %>%
    dist(method = "manhattan")

  dist_matrix <- distances %>%
    as.matrix()

  apply(dist_matrix, 1, function(x) {6-sum(x==1)}) %>%
    sum()

}
