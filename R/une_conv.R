une_conv_mood <- function(relationship = "neutral"){
  df <- data.frame("loved" = c(1, 6, 16, 31, 70, 85, 100),
                   "friendly" = c(2, 8, 20, 40, 76, 89, 100),
                   "peaceful" = c(3, 11, 25, 55, 82, 93, 100),
                   "neutral" = c(5, 15, 30, 60, 85, 95, 100),
                   "distrustful" = c(7, 18, 46, 76, 90, 97, 100),
                   "hostile" = c(11, 24, 61, 81, 93, 98, 100),
                   "hated" = c(15, 30, 69, 84, 94, 99, 100))
  col <- which(names(df) == relationship)
  roll <- sample(1:100, 1)
  moods <- c("withdrawn", "guarded", "cautious", "neutral",
             "sociable", "helpful", "forthcoming")
  return(moods[which(df[,col]  >= roll)[1]])
}

une_conv_bearing <- function(demeanor = sample(1:8, 1)){
  bearings <- data.frame(scheming = c("intent", "bargain", "means",
                                      "proposition", "plan",
                                      "compromise", "agenda",
                                      "arrangement", "negotiation",
                                      "plot"),
                         insane = c("madness", "fear", "accident",
                                    "chaos", "idiocy", "illusion",
                                    "turmoil", "confusion", "façade",
                                    "bewilderment"),
                         friendly = c("alliance", "comfort",
                                      "gratitude", "shelter",
                                      "happiness", "support",
                                      "promise", "delight", "aid",
                                      "celebration"),
                         hostile = c("death", "capture", "judgement",
                                     "combat", "surrender", "rage",
                                     "resentment", "submission",
                                     "injury", "destruction"),
                         inquisitive = c("questions", "investigation",
                                         "interest", "demand",
                                         "suspicion", "request",
                                         "curiosity", "skepticism",
                                         "command", "petition"),
                         knowing = c("report", "effects",
                                     "examination", "records",
                                     "account", "news", "history",
                                     "telling", "discourse",
                                     "speech"),
                         mysterious = c("rumor", "uncertainty",
                                        "secrets", "misdirection",
                                        "whispers", "lies",
                                        "shadows", "enigma",
                                        "obscurity", "conundrum"),
                         prejudiced = c("reputation", "doubt",
                                        "bias", "dislike",
                                        "partiality", "belief",
                                        "view", "discrimination",
                                        "assessment", "difference"),
                         stringsAsFactors = FALSE)

  if(is.numeric(demeanor)){
    return(paste(bearings[sample(1:10,1),
                          demeanor]))
  }else{
    return(bearings[sample(1:10,1),
                    which(names(bearings) == demeanor)])
  }
}

une_conv_focus <- function(){
  focus <- c("current scene","last story", "equipment",
             "parents","history","retainers","wealth",
             "relics","last action","skills"," superiors",
             "fame","campaign","future action","friends",
             "allies","last scene","contacts","flaws","antagonist",
             "rewards"," experience","knowledge","recent scene",
             "community","treasure","the character","current story",
             "family","power","weapons", "previous scene","enemy")
  sample(focus, 1)
}

une_conv <- function(relationship = "neutral",
                     demeanor = sample(1:8,1)){
print(paste("Mood:", une_conv_mood(relationship)))
print(paste("Bearing:", une_conv_bearing(demeanor)))
print(paste("Focus:", une_conv_focus()))
}
