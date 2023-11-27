explore <- function(valve,distances, i=1) {
  visited <- valves[[valve]]$tunnels %>% setdiff(names(distances)[!is.na(distances)])
  distances[visited] <- i
  distances
}

find_distance <- function(start, end, valves) {

  distances <- setNames(rep(NA_integer_, length(valves)),names(valves))

  distances[start] <- 0
  i<-1
  distances <- explore(start,distances,i)
  while (is.na(distances[end])) {
    i<-i+1
    for (start in names(distances)[!is.na(distances)]) {
      distances <- explore(start,distances,i)
    }
  }
  distances[end]
}

make_graph_df <- function(valves) {
  graph_df <- crossing(tibble(start=names(valves)),
           tibble(end=names(valves))) %>%
    mutate(dist = map2_dbl(start,end, find_distance, valves=valves))
  graph_df
}

get_dist <- function(start,end) {

  graph_df <- get(x = "graph_df", envir = rlang::global_env())

  s=start
  e=end

  map_dbl(e, function(e) {
    graph_df %>%
      dplyr::filter(start==s,
                    end==e) %>%
      pull(dist)
  })

}

find_closest <- function(start, ends) {

  graph_df <- get(x = "graph_df", envir = rlang::global_env())

  s=start
  graph_df %>%
    filter(start==s,
           end %in% ends) %>%
    filter(dist == min(dist))
}

find_by_dist <- function(start, dist) {

  graph_df <- get(x = "graph_df", envir = rlang::global_env())

  s=start
  d=dist
  graph_df %>%
    filter(start==s,
           dist==d)
}
