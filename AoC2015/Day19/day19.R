library(tidyverse)

input = read_lines('AoC2015/Day19/input.txt')
input = read_lines('AoC2015/Day19/test')

parse_replacements = function(input) {
  
  s = which(input=="")
  
  replacements = str_split(input[1:(s-1)],' => ', simplify = T)
  replacements = as.data.frame(replacements) %>% 
    group_by(V1) %>% 
    reframe(V2=list(V2))
  replacements = setNames(replacements$V2,replacements$V1)
  replacements
}

parse_molecule = function(input) {
  s = which(input=="")
  molecule=input[(s+1)]
  molecule
}

replacements = parse_replacements(input)
molecule = parse_molecule(input)

# part 1 ----
make_all_molecules = function(molecule, replacements){

  imap(replacements, function(replacement,mol) {
    locs = str_locate_all(molecule, mol)
    map(locs, function(loc) {
      map(replacement, function(r) {
        str_sub(molecule, loc) <- r
        molecule
      }) %>% unlist() %>% unique()
    })
  }) %>% unlist() %>% unique()
}

make_all_molecules(molecule, replacements)  %>% length()

# part 2 ----

# found on reddit
#NumSymbols - #Rn - #Ar - 2 * #Y - 1
str_count(molecule,'[A-Z]') - str_count(molecule, "Rn") - str_count(molecule, "Ar") - 2 * str_count(molecule, "Y") - 1

