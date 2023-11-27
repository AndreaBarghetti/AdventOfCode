library(tidyverse)
library(AoCUtils)

input <- read_aoc_input_with(year = 2019, day = 14, .fun = read_lines)

# input <- read_lines("Day14/test_input2.txt")

parse_input <- function(input) {
  reactions = map(input, function(input) {
    splt = input %>% 
      str_split("=>", simplify = T)
    
    tidy_reaction = function(x) {
      str_extract_all(x, "\\d+", simplify = T)  %>% 
        as.integer() %>% 
        setNames(str_extract_all(x, "[A-Z]+", simplify = T))
    }
    
    left = tidy_reaction(splt[1])
    right = tidy_reaction(splt[2])
    
    list(inputs = left, output = right)
  })
  names(reactions) <- map_chr(reactions,~names(.x$output))
  reactions
}

reactions = parse_input(input)

# part 1 ####
Collector <- R6::R6Class("Collector", 
                         public = list(
                           elements = NA,
                           reactions = NA,
                           ore = 0,
                           level=-1,
                           
                           initialize = function(reactions){
                             self$reactions <- reactions
                             self$elements <- c(c(ORE=0),
                                                setNames(integer(length(reactions)), names(reactions)))
                           },
                           
                           get_requirements = function(element) {
                             
                             self$reactions[[element]]$inputs
                             
                           },
                           
                           collect_for = function(element, amount) {
                             
                             self$level <- self$level+1
                             lv = str_c(rep(". ",self$level),collapse = "")
                             
                             message(lv,"Collect for: ", amount, " of ", element)
                             # if it's ore, just take it, and put it in the debt
                             if (element == 'ORE') {
                               message(lv, "getting ore: +", amount)
                               self$ore = self$ore - amount
                               
                               self$elements[['ORE']] <- self$elements[['ORE']] + amount
                             } else {
                               
                               # if it's not ore
                               # create it until you have enough
                               while (self$elements[[element]] < amount) {
                                 
                                 #list requirements
                                 required = self$get_requirements(element)
                                 
                                 #how much you'll make 
                                 output = unname(self$reactions[[element]]$output)
                                 
                                 # for each requirement, if you have enough use it
                                 iwalk(required, function(a, e){
                                   
                                   if (self$elements[[e]] < a) {
                                     # if you don't have enough collect it first
                                     message(lv, "need to collect more ", e)
                                     self$collect_for(e, a)
                                   }
                                   
                                   if (self$elements[[e]] >= a) {
                                     message(lv, "spending elements: ", a, e)
                                     self$elements[[e]] <- self$elements[[e]] - a
                                     message(lv, e, " is now =", self$elements[[e]])
                                   } 
                                 })
                                 # once all inputs are used, collect output
                                 message(lv, "creating ",element, " +", output)
                                 self$elements[[element]] <- self$elements[[element]] + output
                                 
                               }
                             }
                             
                             self$level <- self$level-1
                           },
                           
                           print = function() {
                             print(self$elements)
                           }
                         )
)

get_fuel_cost <- function(reactions, amount) {
  collector = Collector$new(reactions)
  collector$collect_for("FUEL", 1)
  abs(collector$ore)
}

get_fuel_cost(reactions, 1)

# part 2 ####

# had to use a more efficient strategy to calculate the cost of N FUEL
# this works SOOO much faster also for part 1

CostCalculator <- R6::R6Class("CostCalculator", 
                         public = list(
                           elements = NA,
                           reactions = NA,
                           ore = 0,

                           initialize = function(reactions, amount){
                             self$reactions <- reactions
                             elements <- c(c(ORE=0),
                                                setNames(integer(length(reactions)), names(reactions)))
                             elements['FUEL'] <- -1 * amount
                             self$elements <- elements
                           },
                           
                           run_reaction = function(element, n) {
                             inputs <- self$reactions[[element]]$inputs
                             output <- self$reactions[[element]]$output
                             self$elements[names(inputs)] <- self$elements[names(inputs)] - inputs*n
                             self$elements[element] <- self$elements[element] + output*n
                           },
                           
                           # run the required reactions to restore an element to 0
                           # recursive
                           pay_debt = function(element) {
                             
                             if (element != 'ORE') {
                               
                               amount_required <- -1*self$elements[[element]]
                               if(amount_required<0){amount_required<-0}
                               output = unname(self$reactions[[element]]$output)
                               reactions_required <- amount_required %/% output + sign(amount_required%%output)
                               self$run_reaction(element, reactions_required)
                               
                               reqs <- self$reactions[[element]]$inputs
                               next_debts <- self$elements[names(reqs)] %>% subset(.,.<0)
                               iwalk(next_debts, function(a,e) {
                                 self$pay_debt(e)
                               })
                               
                             } else {
                               amount_required <- -1*self$elements[[element]]
                               self$ore <- self$ore - amount_required
                               self$elements[[element]] <- 0
                             }
                           },

                           print = function() {
                             print(self$elements)
                           }
                         )
)

get_fuel_cost2 <- function(reactions, amount) {
  calculator = CostCalculator$new(reactions,amount)
  calculator$pay_debt("FUEL")
  abs(calculator$ore)
}

get_fuel_cost2(reactions, 1)

# binary search until ore > 1e12
get_max_fuel <- function(reactions,min,max, ore=1e12) {
  
  mid=floor((min+max)/2)
  
  while (max-min>1) {
    test = get_fuel_cost2(reactions, mid)
    if (ore-test > 0) {
      min = mid
      mid=floor((min+max)/2)
    } else {
      max = mid
      mid=floor((min+max)/2)
    }
  }
  
  if (get_fuel_cost2(reactions, max)<=1e12) {
    return(max)
  } else {return(mid)}
  
}
min = (1e12/get_fuel_cost2(reactions, 1)) %/% 2
max = (1e12/get_fuel_cost2(reactions, 1)) * 2
get_max_fuel(reactions, min, max)
