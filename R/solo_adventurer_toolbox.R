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
               "Large encampment, 50-100",
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
                         "In the next realm"), 1)))
 }
 if(subject == "Creature"){
   return("It's about a creature!")
 }
 if(subject == 'Person'){
   return("It's about someone!")
 }
 if(subject == 'Thing'){
   return("It's about something!")
 }
}


