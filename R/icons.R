icons_roll <- function(bonus = 0, difficulty = 0){
  result <- sample(1:6, size = 1) + bonus - sample(1:6, size = 1) - difficulty
  if(result <= -5) string <- "Massive Failure (minus three degrees)"
  if(result %in% c(-3,-4)) string <- "Major Failure (minus two degrees)"
  if(result %in% c(-1,-2)) string <- "Moderate Failure (minus one degree)"
  if(result == 0) string <- "Marginal Success (zero degrees)"
  if(result %in% c(1,2)) string <- "Moderate Success (one degree)"
  if(result %in% c(3,4)) string <- "Major Success (two degrees)"
  if(result >= 5) string <- "Massive Success (three degrees)"
  return(paste(result, string, sep = ", "))
}

icons_slam <- function(damage = 1, strength = 3){
  result <- sample(1:6, size = 1) + damage - sample(1:6, size = 1) - strength
  if(result <= 0) string <- "No Effect"
  if(result %in% c(1,2)) string <- "Knocks target prone. Must spend move action standing up."
  if(result >= 3) string <- "Sends target flying to the next range. Must spend next panel getting up and cannot perform other actions."
  return(paste(result, string, sep = ", "))
}

icons_stun <- function(damage = 1, strength = 3){
  result <- sample(1:6, size = 1) + damage - sample(1:6, size = 1) - strength
  if(result <= 0) string <- "No Effect"
  if(result %in% c(1,2)) string <- "Stuns target for 1 page."
  if(result >= 3) string <- "Reduces target's Stamina to 0 and renders they unconscious."
  return(paste(result, string, sep = ", "))
}

icons_kill <- function(damage = 1, strength = 3){
  result <- sample(1:6, size = 1) + damage - sample(1:6, size = 1) - strength
  if(result <= 0) string <- "No Effect"
  if(result %in% c(1,2)) string <- "Reduces target's Stamina to 0 and renders the character unconscious."
  if(result >= 3) string <- "Reduces target's Stamina to 0 and renders they unconscious. On the following page and each thereafter, they lose a level of Strength, and die when it reaches 0."
  return(paste(result, string, sep = ", "))
}

icons_create_origin <- function(){
 r <- sum(sample(1:6, size = 2, replace = T))
 if(r <=4) return("Trained")
 if(r %in% c(5,6)) return("Transformed")
 if(r == 7) return("Birthright")
 if(r %in% c(8,9)) return("Gimmick")
 if(r == 10) return("Artificial")
 if(r %in% c(11,12)) return("Unearthly")
}

icons_level <- function(r = sum(sample(1:6, size = 2, replace = T))){
  if(r == 2) return(1)
  if(r == 3) return(2)
  if(r == 4) return(3)
  if(r %in% c(5,6)) return(4)
  if(r %in% c(7,8)) return(5)
  if(r %in% c(9,10)) return(6)
  if(r == 11) return(7)
  if(r == 12) return(8)
}

icons_powers_number <- function(){
 r <- sum(sample(1:6, size = 2, replace = T))
 if(r <= 4) return(2)
 if(r <= 7) return(3)
 if(r <= 10) return(4)
 return(5)
}

icons_power_type_table <- function(){
 r <- sum(sample(1:6, size = 2, replace = T))
 if(r <=3) return("Mental")
 if(r <=5) return("Control")
 if(r == 6) return("Defensive")
 if(r == 7) return("Offensive")
 if(r == 8) return("Movement")
 if(r <= 10) return("Alteration")
 return("Sensory")
}

icons_power_alteration <- function(){
  sample(c("Ability Boost (group)",
           "Ability Increase (group)",
           "Alter Ego",
           "Alternate Form (group)",
           "Aquatic",
           "Density",
           "Dimension Control",
           "Duplication",
           "Evolution",
           "Extra Body Parts (group)",
           "Gestalt",
           "Growth",
           "Invisibility",
           "Mimicry (group)",
           "Phasing",
           "Shrinking",
           "Stretching",
           "Transformation"), size = 1)
}

icons_power_control <- function(){
  r <- sample(1:18, size = 1)
  if(r <= 2) return("Alteration Ray (group)")
  if(r <= 4) return("Continuum Control (group)")
  if(r <= 6) return("Element Control (group)")
  if(r <= 8) return("Energy Control (group)")
  if(r <= 9) return("Healing")
  if(r <= 10) return("Machine Control")
  if(r <= 12) return("Matter Control (group)")
  if(r <= 14) return("Power Control (group)")
  if(r <= 15) return("Servant")
  if(r <= 16) return("Spirit Control")
  if(r <= 18) return("Wizardry (group)")
}

icons_power_defensive <- function(){
  r <- sample(1:18, size = 1)
  if(r <= 2) return("Absorption")
  if(r <= 3) return("Adaptation")
  if(r <= 6) return("Force Field")
  if(r <= 7) return("Immortality")
  if(r <= 10) return("Life Support")
  if(r <= 12) return("Reflection")
  if(r <= 14) return("Regeneration")
  if(r <= 18) return("Resistance (group)")
}

icons_power_mental <- function(){
  r <- sample(1:12, size = 1)
  if(r <= 1) return("Astral Projection")
  if(r <= 2) return("Dream Control")
  if(r <= 4) return("Emotion Control")
  if(r <= 6) return("Illusion or Images")
  if(r <= 8) return("Mental Blast")
  if(r <= 9) return("Mind Control")
  if(r <= 10) return("Mind Shield")
  if(r <= 12) return("Telepathy")
}

icons_power_movement <- function(){
  r <- sample(1:12, size = 1)
  if(r <= 1) return("Burrowing")
  if(r <= 2) return("Dimensional Travel")
  if(r <= 4) return("Flight")
  if(r <= 6) return("Leaping")
  if(r <= 7) return("Spinning")
  if(r <= 9) return("Super-Speed")
  if(r <= 10) return("Swinging")
  if(r <= 11) return("Teleportation")
  if(r <= 12) return("Wall-Crawling")
}

icons_power_offensive <- function(){
  r <- sample(1:12, size = 1)
  if(r <= 1) return("Affliction")
  if(r <= 2) return("Aura")
  if(r <= 3) return("Binding")
  if(r <= 5) return("Blast")
  if(r <= 6) return("Corrosion")
  if(r <= 7) return("Dazzle")
  if(r <= 8) return("Energy Drain")
  if(r <= 9) return("Fast Attack")
  if(r <= 11) return("Strike")
  if(r <= 12) return("Stunning")
}

icons_power_sensory <- function(){
  r <- sample(1:12, size = 1)
  if(r <= 2) return("Danger Sense")
  if(r <= 4) return("Detection (group)")
  if(r <= 5) return("Environmental Awareness")
  if(r <= 6) return("ESP")
  if(r <= 7) return("Interface")
  if(r <= 8) return("Postcognition")
  if(r <= 9) return("Precognition")
  if(r <= 12) return("Super-Senses (group)")
}

icons_specialties <- function(){
  r <- sum(sample(1:6, size = 2, replace = T))
  if(r <= 4) return(1)
  if(r <= 7) return(2)
  if(r <= 10) return(3)
  if(r <= 12) return(4)
}

icons_specialty <- function(){
 r <- sample(1:36, size = 1)
 if(r == 1) return("Aerial Combat")
 if(r == 2) return("Art")
 if(r <= 4) return("Athletics")
 if(r == 5) return("Business")
 if(r == 6) return("Drive")
 if(r <= 8) return("Investigation")
 if(r <= 9) return("Law")
 if(r <= 11) return("Leadership")
 if(r <= 12) return("Linguistics")
 if(r <= 14) return("Martial Arts")
 if(r <= 15) return("Medicine")
 if(r <= 17) return("Mental Resistance")
 if(r <= 18) return("Military")
 if(r <= 19) return("Occult")
 if(r <= 20) return("Performance")
 if(r <= 21) return("Pilot")
 if(r <= 24) return("Power")
 if(r <= 25) return("Psychiatry")
 if(r <= 27) return("Science")
 if(r <= 28) return("Sleight of Hand")
 if(r <= 30) return("Stealth")
 if(r <= 32) return("Technology")
 if(r <= 33) return("Underwater Combat")
 if(r <= 35) return("Weapons")
 if(r <= 36) return("Wrestling")
}





