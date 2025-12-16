library(tidyverse)

input = read_lines('AoC2015/Day22/input.txt')

boss = str_extract_all(input,'\\d+') %>% as.numeric() %>% setNames(nm = c('HP','D'))

# part 1 ----

init_fight = function(boss) {
  boss['P']=0 #poison status
  
  you = c(HP=50,M=500,A=0)
  you['S'] = 0 # shield status
  you['R'] = 0 # recharge status
  you['C'] = 0 # spent mana

  fight = list(you  = you,
               boss = boss,
               turn=1)
  fight
}

spend_mana = function(fight, mana) {
  fight$you['M'] = fight$you['M'] - mana
  fight$you['C'] = fight$you['C'] + mana
  fight
}

MagicMissile = function(fight) {
  fight = spend_mana(fight, 53)
  fight$boss['HP'] = fight$boss['HP']-4
  fight
}

Drain = function(fight) {
  fight = spend_mana(fight, 73)
  fight$boss['HP'] = fight$boss['HP'] - 2
  fight$you['HP'] = fight$you['HP'] + 2
  fight
}

Shield = function(fight) {
  fight = spend_mana(fight, 113)
  fight$you['S'] = 6
  fight
}

Poison = function(fight) {
  fight = spend_mana(fight, 173)
  fight$boss['P'] = 6
  fight
}

Recharge = function(fight) {
  fight = spend_mana(fight, 229)
  fight$you['R'] = 5
  fight
}

start_turn = function(fight) {
  fight$turn = fight$turn + 1
  
  #apply effects
  if (fight$boss['P']>0) {fight$boss['HP'] = fight$boss['HP']-3}
  if (fight$you['S']>0) {fight$you['A'] = 7} else {fight$you['A']=0}
  if (fight$you['R']>0) {fight$you['M'] = fight$you['M'] + 101}

  #decrease counters
  fight$boss['P'] = max(0, fight$boss['P']-1)
  fight$you['S'] = max(0, fight$you['S']-1)
  fight$you['R'] = max(0, fight$you['R']-1)

  fight
}

boss_attack = function(fight) {
  fight$you['HP'] = fight$you['HP'] - max(1, fight$boss['D'] - fight$you['A'])
  fight
}

you_attack = function(fight, spell) {
  fight = do.call(spell, list(fight))
  fight
}

get_avail_spells = function(fight) {
  spells = c(MagicMissile=53, 
             Drain=73, 
             Shield=113,
             Poison=173,
             Recharge=229)
  
  spells = spells <= fight$you['M']
  
  if (fight$boss['P']>0) {spells['Poison']=F}
  if (fight$you['R']>0) {spells['Recharge']=F}
  if (fight$you['S']>0) {spells['Shield']=F}
  names(spells)[spells]
}

library(collections)

try_all_fights = function(boss) {

  fight = init_fight(boss)
  
  q = stack()
  
  q$push(fight)
  
  min_mana_spent = Inf
  
  while(q$size()>0) {
    
    fight = q$pop()
    
    if (fight$you['C']>=min_mana_spent) {next}
    
    if (fight$boss['HP']<=0) {min_mana_spent = min(min_mana_spent, fight$you['C']); next}
    if (fight$you['HP']<=0) {next}
    
    fight = start_turn(fight)
    if (fight$boss['HP']<=0) {min_mana_spent = min(min_mana_spent, fight$you['C']); next}
    if (fight$you['HP']<=0) {next}
    
    if (fight$turn%%2==1) {
      fight = boss_attack(fight)
      q$push(fight)
    } else {
      avail_spells = get_avail_spells(fight)
      for (sp in avail_spells) {
        next_f = you_attack(fight, sp)
        q$push(next_f)
      }
    }
  }
  min_mana_spent
}

try_all_fights(boss)

# part 2 ----
start_turn_pt1 = start_turn

start_turn = function(fight) {
  
  if (fight$turn%%2==1) {
    fight$you['HP'] = fight$you['HP'] - 1
    if (fight$you['HP']<=0) {
      return(fight)
    }
  }
  
  fight = start_turn_pt1(fight)
  fight
}

try_all_fights(boss)
