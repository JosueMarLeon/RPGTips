#' @export
toolbox_weather <- function(season = "autumn"){
  dice <- sample(1:20, size = 1)
  if(season == "summer"){
    choices <- c("Unseasonably Cold",
                 rep("Rainy", times = 3),
                 rep("Overcast", times = 4),
                 rep("Clear, Warm", times = 3),
                 rep("Clear, Hot", times = 7),
                 rep("Very Hot", times = 2))
    return(sample(choices, size = 1))
  }
  if(season == "autumn"){
    choices <- c("Sleet / Blizzard",
                 rep("Rainy", times = 3),
                 rep("Light Rain", times = 4),
                 rep("Overcast", times = 3),
                 rep("Clear Skies", times = 7),
                 rep("Hot", times = 2))
    return(sample(choices, size = 1))
  }
  if(season == "winter"){
    choices <- c("Heavy Snow",
                 rep("Snowstorm", times = 3),
                 rep("Rain", times = 4),
                 rep("Overcast", times = 3),
                 rep("Clear Skies", times = 7),
                 rep("Unreasonably Warm", times = 2))
    return(sample(choices, size = 1))
  }
  if(season == "spring"){
    choices <- c("Sleet / Blizzard",
                 rep("Rain", times = 3),
                 rep("Overcast", times = 4),
                 rep("Clear Skies", times = 3),
                 rep("Warm", times = 7),
                 rep("Hot", times = 2))
    return(sample(choices, size = 1))
  }
}

#' @export
toolbox_settlement <- function(){
  sample(x = c("Large camp / caravan (25% abandoned), 1-20",
               "Cottage, 1-10",
               "Large encampent, 50-100",
               "Hamlet, 50-150",
               "Work crew, 50-200",
               "Stationed garrison, 100-300",
               "Fort, 200-400",
               "Village, 300-1000",
               "Encamped Army, 1000-3000",
               "Refugee encampent, 500-5000"),
         size = 1)
}

#' @export
toolbox_disturbance <- function(){
  tmp <- data.frame(num = c(20,35,50,60,80,85,90,91,100),
                    dist = c("Loud Noise",
                             "Animal (indifferent)",
                             "Animal (hostile)",
                             "Disturbing dream / vision",
                             "NPC (curious)",
                             "Bandit / Thief",
                             "Monster",
                             "Natural Distaster",
                             "Storm / Weather Change"),
                    stringsAsFactors = F)
  dice <- sample(1:100, size = 1)
  tmp$dist[which(tmp$num >= dice)[1]]
}

#' @export
toolbox_tavern <- function(){
  rooms <- c("None", 1:5, 6, 8, 10, 20)
  quality <- c("Atrocious",
               rep("Poor", times = 2),
               rep("Average", times = 3),
               rep("Good", times = 2),
               "Excellent",
               "Outstanding")
  rumours <- c("None", 1,1,2,2,2,3,3,4,4)
  customer_service <- c("Unfriendly",
                        "Neutral",
                        "Neutral",
                        "Civil",
                        "Civil",
                        "Cordial",
                        "Cordial",
                        "Warm & Welcoming",
                        "Warm & Welcoming",
                        "Treated like a monarch")
  res <- list()
  res[[1]] <- paste0("Rooms: ", sample(rooms,1))
  res[[2]] <- paste0("Quality: ", sample(quality,1))
  res[[3]] <- paste0("Rumours: ", sample(rumours, 1))
  res[[4]] <- paste0("Customer service: ", sample(customer_service, 1))
  res[[5]] <- paste0("Appearance: ", gme_description_table())
  res[[6]] <- paste0("Innkeep: ", gme_description_table())
  cat(paste(res, sep = "\n", collapse = "\n"))
}

#' @export
toolbox_rumour <- function(){
 subject <- sample(c("Person", "Creature", "Place", "Thing"), 1)
 if(subject == "Place"){
   return(paste("Place:",
                sample(c("In this settlement/area",
                         "Just outside settlement",
                         "1d4 miles away, in a structure",
                         "In the nearest forest",
                         "In the nearest hills",
                         "In the nearest mountains",
                         "In the nearest swamp",
                         "In/beside the nearest body of water",
                         "In the next village",
                         "In the next large town",
                         "In the capital of the realm",
                         "In the next realm"))))
 }
}

#' @export
toolbox_terrain_arctic <- function(){
  tmp <- data.frame(num = c(60,65,70,75,80,85,90,95,100),
                   feat = c("Continuing arctic terrain, 5 miles",
                           "Icy hills (roll hills and interpret... arctically). 1d4 m.",
                           "Small mountain range / foothills. 1d4 m.",
                           "Waterway (probably frozen). 1d4 m",
                           "Small wood. 1d4 m.",
                           "Rocky outcrop. 1d4 m.",
                           "Unmarked settlement",
                           "D6: structure / monument / interesting landscape feature",
                           "Monster Lair"), stringsAsFactors = F)
  dice <- sample(1:100, size = 1)
  tmp$feat[which(tmp$num >= dice)[1]]
}

#' @export
toolbox_terrain_coastal <- function(){
  tmp <- data.frame(num = c(60,65,70,75,80,85,90,95,100),
                    feat = c("Continuing coastal terrain, 5 miles",
                             "Hills, with cliffs perhaps. 1d4 m.",
                             "Gully. 1d4 m.",
                             "Waterway. 1d4 m",
                             "Small wood. 1d4 m.",
                             "Rocky outcrop. 1d4 m.",
                             "Unmarked settlement",
                             "D6: structure / monument / interesting landscape feature",
                             "Monster Lair"), stringsAsFactors = F)
  dice <- sample(1:100, size = 1)
  tmp$feat[which(tmp$num >= dice)[1]]
}

#' @export
toolbox_terrain_desert <- function(){
  tmp <- data.frame(num = c(70,75,80,85,90,95,100),
                    feat = c("Continuing desert, 5 miles",
                             "Oasis (roll Oasis table). 1d4 m.",
                             "Small mountain range (dry, no snow). 1d4 m.",
                             "Rocky outcrop. 1d4 m",
                             "Unmarked settlement",
                             "D6: structure / monument / interesting landscape feature",
                             "Monster Lair"), stringsAsFactors = F)
  dice <- sample(1:100, size = 1)
  tmp$feat[which(tmp$num >= dice)[1]]
}

#' @export
toolbox_terrain_forest <- function(){
  tmp <- data.frame(num = c(52,56,60,64,68,72,76,80,84,88,92,96,100),
                    feat = c("Continuing forest, 5 miles",
                             "Small clearing",
                             "Large clearing",
                             "Small wooded gully, 1d4 m.",
                             "Large wooded gully, 1d4 m.",
                             "Waterway",
                             "Lake, 1d4 m.",
                             "Clearfelled area, 1d4 m.",
                             "Rocky outcrop, 1d4 m.",
                             "Swamp, 1d4 m.",
                             "Unmarked forest settlement",
                             "D6: structure / monument / monster lair",
                             "Interesting Landscape feature"), stringsAsFactors = F)
  dice <- sample(1:100, size = 1)
  tmp$feat[which(tmp$num >= dice)[1]]
}

#' @export
toolbox_terrain_grassland <- function(){
  tmp <- data.frame(num = c(56,60,64,68,72,76,80,84,88,92,96,100),
                    feat = c("Continuing grassland, 5 miles",
                             "Hills",
                             "Swamp",
                             "Gully, 1d4 m.",
                             "Waterway, 1d4 m.",
                             "Lake",
                             "Small wood, 1d4 m.",
                             "Rocky outcrop, 1d4 m.",
                             "Small mountain range / foothills, 1d4 m.",
                             "Unmarked settlement",
                             "D6: structure / monument / interesting landscape feature",
                             "Monster Lair"), stringsAsFactors = F)
  dice <- sample(1:100, size = 1)
  tmp$feat[which(tmp$num >= dice)[1]]
}

#' @export
toolbox_terrain_hills <- function(){
  tmp <- data.frame(num = seq(55, 100, by = 5),
                    feat = c("Continuing hills, 5 miles",
                             "Gully, 1d4 m.",
                             "Waterway, 1d4 m.",
                             "Lake, 1d4 m.",
                             "Small wood, 1d4 m.",
                             "Rocky outcrop, 1d4 m.",
                             "Small mountain range / Foothills, 1d4 m.",
                             "Unmarked settlement",
                             "D6: structure / monument / monster lair",
                             "Interesting Landscape feature"), stringsAsFactors = F)
  dice <- sample(1:100, size = 1)
  tmp$feat[which(tmp$num >= dice)[1]]
}

#' @export
toolbox_terrain_mountains <- function(){
  tmp <- data.frame(num = seq(55, 100, by = 5),
                    feat = c("Continuing mountains, 5 miles",
                             "Gully, 1d4 m.",
                             "Waterway, 1d4 m.",
                             "Lake, 1d4 m.",
                             "Small wood, 1d4 m.",
                             "Rocky outcrop, 1d4 m.",
                             "Small mountain range / Foothills, 1d4 m.",
                             "Unmarked settlement",
                             "D6: structure / monument / monster lair",
                             "Interesting Landscape feature"), stringsAsFactors = F)
  dice <- sample(1:100, size = 1)
  tmp$feat[which(tmp$num >= dice)[1]]
}

#' @export
toolbox_terrain_swamp <- function(){
  tmp <- data.frame(num = c(51,58,65,72,79,86,93,100),
                    feat = c("Continuing swamp, 5 miles",
                             "Waterway, 1d4 m.",
                             "Lake, 1d4 m.",
                             "Small wood, 1d4 m.",
                             "Rocky outcrop, 1d4 m.",
                             "Unmarked settlement",
                             "D6: structure / monument / monster lair",
                             "Interesting Landscape feature"), stringsAsFactors = F)
  dice <- sample(1:100, size = 1)
  tmp$feat[which(tmp$num >= dice)[1]]
}


