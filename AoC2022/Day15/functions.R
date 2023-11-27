read_input <- function(input) {

  map(input, function(input) {
    ls <- input[[1]] %>%
      str_split(":", simplify = T)

    coordinates <- setNames(ls, c("sensor", "beacon")) %>%
      map(function(x) {
        x %>% str_extract_all("[-+]*\\d+", simplify = T) %>%
          as.integer()
      })

    return(coordinates)
  })
}

get_coord_df <- function(coordinates) {
  sensor_x <- coordinates %>% map_dbl(~.x[[1]][1])
  sensor_y <- coordinates %>% map_dbl(~.x[[1]][2])
  beacon_x <- coordinates %>% map_dbl(~.x[[2]][1])
  beacon_y <- coordinates %>% map_dbl(~.x[[2]][2])

  df <- tibble(sensor_x=sensor_x,
               sensor_y=sensor_y,
               beacon_x=beacon_x,
               beacon_y=beacon_y)
  tidy_df <- df %>%
    mutate(i = row_number()) %>%
    pivot_longer(cols = 1:4, names_to = c("center","axis"),names_sep = "_", values_to = "value") %>%
    spread(axis, value) %>%
    group_by(i) %>%
    mutate(dist_x = abs(diff(x)),
           dist_y = abs(diff(y)),
           dist = dist_x+dist_y) %>%
    ungroup()
}

scan_row <- function(set_y, coord_df) {
  coord_df %>%
    filter(center=="sensor") %>%
    mutate(overlap = dist-abs(y-set_y)) %>%
    filter(overlap>0) %>%
    pmap(function(overlap, x,...) {
      range(x-overlap,x+overlap)
    }) %>%
    purrr::reduce(rbind) %>%
    reduce_ranges()
}

get_corners <- function(center, dist, addx=0) {
  dist=dist+addx
  c1 <- c(center[1]+dist,center[2])
  c2 <- c(center[1],center[2]-dist)
  c3 <- c(center[1]-dist,center[2])
  c4 <- c(center[1],center[2]+dist)
  mat <- rbind(c1,c2,c3,c4)
  tibble(x=mat[,1],y=mat[,2])
}

man_dist <- function(pos1,pos2) {
  sum(abs(pos1-pos2))
}

man_edge_dist <- function(sensor1, sensor2) {
  man_dist(sensor1[1:2],sensor2[1:2])-sum(sensor1[3],sensor2[3])
}

# get intersect between 2 segments
cross_segments <- function(segment1, segment2) {

  A = segment1$start
  B = segment1$end
  C = segment2$start
  D = segment2$end

  slope1 = (B[1]-A[1])/(B[2]-A[2])
  slope2 = (D[1]-C[1])/(D[2]-C[2])

  # if parallel
  if (slope1==slope2) {return()}

  k1 = A[2] - A[1] * slope1
  k2 = D[2] - D[1] * slope2

  x =  (k2 - k1)/(slope1-slope2)
  y =  x * slope1 + k1

  xrange = range(A[1],B[1])
  yrange = range(C[2],D[2])
  if (!between(x,xrange[1],xrange[2])) {return()}
  if (!between(y,yrange[1],yrange[2])) {return()}

  return(c(x=x,y=y))
}

# segment1 = list(end=c(1,-1), start=c(5,3))
# segment2 = list(end=c(2,4), start=c(6,0))
# cross_segments(segment1, segment2)

# get all edges of a sensor
# as segments
get_edges <- function(sensor) {
  corners = get_corners(sensor[1:2],sensor[3])

  map2(c(4,1,2,3),c(1,2,3,4), function(start,end) {
    segment = list(start=as.matrix(corners[start,]),end=as.matrix(corners[end,]))
  })
}


# get intersections between two sensors edges
get_intersections <- function(sensor1, sensor2) {
  edges_1 <- get_edges(sensor1)
  edges_2 <- get_edges(sensor2)

  map(edges_1, function(edge1) {
    map(edges_2, function(edge2) {
      cross_segments(edge1,edge2)
    }) %>% purrr::reduce(bind_rows)
  }) %>% purrr::reduce(bind_rows)
}


reduce_ranges <- function(ranges) {
  IRanges::IRanges(start = ranges[,1],end=ranges[,2]) %>%
    IRanges::reduce() %>%
    {cbind(IRanges::start(.), IRanges::end(.))}
}

# check if given position is within the sensors range
check_coverage <- function(pos, sensor_df) {
  sensor_df %>%
    group_by(i) %>%
    mutate(is_within = man_dist(pos,c(x,y))-dist<=0)
}

# check if given position is within the sensors range
# return only T or F
is_covered <- function(pos,coord_df) {
  check_coverage(pos,coord_df) %>%
    pull(is_within) %>% any()
}


plot_space <- function(coord_df,target_range, center_on="sensor") {
  coord_df %>%
    mutate(dist = ifelse(center==center_on,dist,0)) %>%
    group_by(i,center) %>%
    summarise(corners = get_corners(c(x,y),dist,add=.5)) %>%
    unique() %>%
    ggplot(aes(x=corners$x,y=corners$y, fill=center)) +
    geom_polygon(aes(group=interaction(center,i), fill=center), alpha=.3, linewidth=0,
                 show.legend = F) +
    theme_void() +
    coord_equal() +
    annotate(geom = 'rect', ymin=target_range[1],ymax=target_range[2],
             xmin=target_range[1],xmax=target_range[2], fill="transparent",
             col="black", linewidth=.1) +
    geom_point(data=coord_df, aes(x=x,y=y, fill=center),
               alpha=.1,
               shape=22,
               show.legend = F) +
    scale_fill_manual(values=c("red","green"))
}


