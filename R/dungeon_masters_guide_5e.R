## Content from the DMG from 5e

#' @export
dmg_world_shaking_event <- function(){
  roll1 <- sample(1:10, 1)
  leader <- c("Political", "Religious", "Military",
              "Crime/underworld", "Art/culture", "Philosophy/learning/magic")
  disasters <- c("Earthquake", "Famine/drought", "Fire", "Flood",
                 "Plague/disease", "Rain of fire (meteoric impact)",
                 "Storm (hurricane, tornado, tsunami)", "Volcanic eruption", "
                 Magic gone awry or a planar warp", "Divine judgment")
  invaders <- c("A criminal enterprise", "Monsters or a unique monster",
                "A planar threat", "A past adversary reawakened, reborn or resurgent",
                "A splinter faction", "A savage tribe", "A traitorous ally")
  extinct <- c("A kind of animal", "Habitable land", "Magic or magic-users",
               "A mineral resource", "A type of monster", "A people", "A kind of plant",
               "A waterway")
  organization <- c("Crime syndicate/bandit confederacy",
                    "Guild (masons, apothecaries, goldsmiths)",
                    "Magical circle/society",
                    "Military/knightly order",
                    "New family dynasty/tribe/clan",
                    "Philosophy/discipline dedicated to a principle or ideal",
                    "Realm (village, town, duchy, kingdom)",
                    "Religion/sect/denomination",
                    "School/university",
                    "Secret society/cult/cabal")
  discovery <- c("Ancient ruin/lost city of a legendary race",
                 "Animal/monster/magical mutation",
                 "Invention/technology/magic (helpful, destructive)",
                 "New (or forgotten) god or planar entity",
                 "New (or rediscovered) artifact or religious relic",
                 "New land (island, continent, lost world , demiplane)",
                 "Otherworldly object (planar portal, alien spacecraft)",
                 "People (race, tribe, lost civilization , colony)",
                 "Plant (miracle herb, fungal parasite, sentient plant)",
                 "Resource or wealth (gold, gems, mithral)")
  result <- switch(as.character(roll1),
                   "1" = paste0("Rise of a leader or era: ", sample(leader, 1)),
                   "2" = paste0("Fall of a leader or era: ", sample(leader, 1)),
                   "3" = paste0("Cataclysmic disaster: ", sample(disasters, 1)),
                   "4" = paste0("Assault or invasion: ", sample(invaders, 1)),
                   "5"= "Rebellion, revolution, overthrow",
                   "6" = paste0("Extinction or depletion: ", sample(extinct, 1)),
                   "7" = paste0("New organization: ", sample(organization, 1)),
                   "8" = paste0("Discovery, expansion, invention: ", sample(discovery, 1)),
                   "9" = "Prediction, omen, prophecy",
                   "10" = "Myth and legend (roll again and make it BIG)")
  result
}

