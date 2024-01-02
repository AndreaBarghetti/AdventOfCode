library(tidyverse)
input <- read_lines("AoC2023/Day20/input.txt")

parse_input <- function(input) {
  
  types = str_extract(input, "^[%&]") %>% replace_na("Broadcaster")
  names = str_extract(input, "\\w+")
  dests = str_remove(input, ".+-> ") %>% str_split(", ")
  types = recode(types,"%"="Flipflop","&"="Conjunction")
  pmap(list(types,dests), function(t,d) {
    list(type=t,dests = d, inputs = list())
  }) %>% setNames(names)
}

modules = parse_input(input)

iwalk(modules,function(m,id) {
  walk(m$dests, ~{
    if(!is.null(modules[[.x]])){
      modules[[.x]]$inputs <<- c(modules[[.x]]$inputs, id)
    }
  })
})

modules$`button module` <- list(type = "button module", dests="broadcaster",inputs=NULL)

# Part 1 ####
Module <- R6::R6Class("Module",
                      public = list(
                        id=NA,
                        type=NA,
                        dests = NA,
                        inputs = NA,
                        status = 0,
                        l_pulses = 0,
                        h_pulses = 0,
                        records = list(),
                       
                        initialize = function(id, type, dests, inputs) {
                          self$id=id
                          self$type=type
                          self$dests=dests
                          self$inputs=rep(0, length(inputs)) %>% setNames(inputs)
                        },
                        
                        record_pulse = function(pulse){
                          if (pulse==0) {
                            self$l_pulses <- self$l_pulses+length(self$dests)
                          }
                          if (pulse==1) {
                            self$h_pulses <- self$h_pulses+length(self$dests)
                          }
                        },
                        
                        run = function(pulse, id,counter=0) {
                          
                          self$inputs[[id]] <- pulse
                          
                          if (self$type=="button module") {
                            
                            self$record_pulse(pulse)
                            
                            return(list(from=self$id,
                                        to=self$dests,
                                        pulse=0))
                          }
                          if (self$type=="Broadcaster") {
                            
                            self$record_pulse(pulse)
                            
                            return(list(from=self$id,
                                        to=self$dests,
                                        pulse=pulse))
                          }
                          if (self$type=="Flipflop") {
                            if (pulse==0) {
                              self$status  <- ifelse(self$status==0,1,0)
                              pulse = self$status
                              
                              self$record_pulse(pulse)
                              
                              return(list(from=self$id,
                                          to=self$dests,
                                          pulse=pulse))
                            }
                          }
                          if (self$type=="Conjunction") {
                            
                            if(self$id=='vr'){
                              if(any(self$inputs==1)){
                                self$records = c(self$records,list(c(self$inputs,i=counter)))
                              }
                            }
                            
                            self$inputs[[id]] <- pulse
                            pulse = ifelse(all(self$inputs == 1),0,1)
                            
                            self$record_pulse(pulse)
                            
                            return(list(from=self$id,
                                        to=self$dests,
                                        pulse=pulse))
                          }
                        },
                        
                        print = function() {
                          cat(self$id,self$type)
                          if (self$type == "Flipflop") {cat(" status: ",self$status)}
                          if (self$type == "Conjunction") {cat(" inputs:\n",names(self$inputs),"\n", self$inputs)}
                        }
                      )
)

Modules <- imap(modules, function(module,id) {
  M <- Module$new(id = id, type = module$type, dests = module$dests, inputs = module$inputs)
  M
})

push_button_once <- function(Modules, counter) {
  queue = collections::queue()
  queue$push(list(to = 'button module', pulse = -1, from=""))
  
  while (queue$size()>0) {
    cmd = queue$pop()
    if(!is.null(Modules[[cmd$to]])) {
      next_cmds = Modules[[cmd$to]]$run(pulse = cmd$pulse, id = cmd$from, counter=counter)
      for (dest in next_cmds$to) {
        queue$push(list(to=dest, pulse =next_cmds$pulse, from = next_cmds$from))
      }
    }
  }
}

push_button <- function(Modules, n) {
  for (i in 1:n) {
    # print(i)
    push_button_once(Modules, counter=i)
  }
}

push_button(Modules,1000)

prod(sum(map_dbl(Modules,~.x$l_pulses)), sum(map_dbl(Modules,~.x$h_pulses))) %>% 
  format(scientific = F)

# Part 2 ####
Modules <- imap(modules, function(module,id) {
  M <- Module$new(id = id, type = module$type, dests = module$dests, inputs = module$inputs)
  M
})

names(Modules[map_lgl(Modules, ~"rx"%in%.x$dests)])

push_button(Modules,10000)

loops = Modules[['vr']]$records %>% 
  map_dfr(~.x) %>% 
  gather(m,val,all_of(names(Modules[['vr']]$inputs))) %>% 
  unique() %>%
  filter(val==1) %>% 
  group_by(m) %>% 
  arrange(i) %>% 
  slice(1) %>% 
  pull(i)

numbers::mLCM(loops) %>% 
  format(scientific=F)
