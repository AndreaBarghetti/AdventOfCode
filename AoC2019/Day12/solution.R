library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 12, .fun = read_lines)

input <- read_lines('Day12/test_input2.txt')

get_moon_status <- function(input) {
  map(setNames(input,c("I","E","G","C")),~{
    position = str_extract_all(.x, "[-+]*[\\d]+", simplify = T) %>% 
      as.integer() %>% setNames(c("x","y","z"))
    velocity = c(x=0,y=0,z=0)
    gravity = c(x=0,y=0,z=0)
    list(position=position, velocity=velocity, gravity = gravity)
  })
}

moon_status = get_moon_status(input)


# part 1 ####
System = R6::R6Class("System",
                     public = list(
                       
                       pairs = combn(names(moon_status),2),
                       moon_status = NA,
                       moons=NA,
                       time=0,

                       initialize = function(moon_status) {
                         self$moon_status = moon_status
                         self$moons = names(moon_status)
                       },
                       
                       apply_gravity = function() {
                         walk2(self$pairs[1,],self$pairs[2,],function(m1,m2) {
                           lower <- self$moon_status[[m1]]$position < self$moon_status[[m2]]$position
                           equals <- self$moon_status[[m1]]$position == self$moon_status[[m2]]$position
                           m1change <- ifelse(lower,1,-1); m1change[equals]<-0
                           m2change <- m1change*(-1)
                           self$moon_status[[m1]]$gravity <- self$moon_status[[m1]]$gravity+m1change
                           self$moon_status[[m2]]$gravity <- self$moon_status[[m2]]$gravity+m2change
                         })
                       },
                       
                       reset_gravity = function() {
                         walk(self$moons, function(m) {
                           self$moon_status[[m]]$gravity <- c(x=0,y=0,z=0)
                         })
                       },
                       
                       apply_velocity = function() {
                         walk(self$moons, function(m) {
                           self$moon_status[[m]]$velocity <- self$moon_status[[m]]$velocity + self$moon_status[[m]]$gravity
                         })
                       },
                       
                       move = function() {
                         walk(self$moons, function(m) {
                           self$moon_status[[m]]$position <- self$moon_status[[m]]$position + self$moon_status[[m]]$velocity
                         })
                       },
                       
                       simulate = function(time) {
                         
                         while (self$time != time) {
                           self$time <- self$time + 1
                           self$apply_gravity()
                           self$apply_velocity()
                           self$move()
                           self$reset_gravity()
                         }
                         
                       },
                       
                       moon_energy = function(moon) {
                         potential = sum(abs(self$moon_status[[moon]]$position))
                         kinetic = sum(abs(self$moon_status[[moon]]$velocity))
                         return(potential*kinetic)
                       },
                       
                       total_energy = function() {
                         map_dbl(self$moons, function(moon) {
                           self$moon_energy(moon)
                         }) %>% sum()
                       },
                       
                       print = function() {
                         walk(self$moons, function(m) {
                           cat(self$moon_status[[m]]$position," ; ",self$moon_status[[m]]$velocity)
                           cat("\n")
                         })
                       }
                       
                     )
)

jupiter <- System$new(moon_status)

jupiter$simulate(1000)
jupiter$total_energy()

# part 2 ####
System$set("public", "initial_status",overwrite=T, {
  initial_status <- moon_status %>% 
    map_dfr(~rbind(.x$position, .x$velocity) %>% as_tibble())
  initial_status
})

System$set("public", "periods", {
  c(x=NA,y=NA, z=NA)
})

System$set("public", "check_period", function() {
  
  status <- self$moon_status %>% 
    map_dfr(~rbind(.x$position, .x$velocity) %>% as_tibble())

  unfinished <- names(self$periods)[is.na(self$periods)]
 
  for (d in unfinished) {
    if(isTRUE(all.equal(status[[d]], initial_status[[d]]))) {
      self$periods[d] <- self$time
    }
  }

})

System$set("public", "loop", function() {
  
  while (anyNA(self$periods)) {
    
    self$simulate(self$time + 1)
    self$check_period()
  } 
  
  self$periods <- as.numeric(self$periods)
  return(
    gmp::lcm.bigz(self$periods[1],self$periods[2]) %>% gmp::lcm.bigz(self$periods[3])
  )
})

jupiter <- System$new(moon_status)
jupiter$loop()
