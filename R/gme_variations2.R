# GME Variations 2 Fate Check ----
#' @export
gme_fate_check <- function(chaosFactor = 4,chaosAdjust = 2,prob = 0){
  throw1 <- sample(1:10,1)
  throw2 <- sample(1:10,1)
  throw <-  throw1 + throw2 + prob
  if(chaosFactor %in% c(3,6)){
    throw <- throw + chaosAdjust
  }
  answer <- ifelse(test = throw >= 11, yes = "Yes", no = "No")
  chaosDice <- sample(1:10,1)
  if(chaosDice <= chaosFactor & throw1 == throw2){
    answer <- paste("Random Event and Exceptional", answer)
  }else{
    if(throw1%%2==0 & throw2%%2==0){
      answer <- paste("Random Event and", answer)
    }
    if(throw1%%2!=0 & throw2%%2!=0){
      answer <- paste("Exceptional", answer)
    }
  }
  return(answer)
}

# GME Variations 2 Detail Check ----
#' @export
gme_detail_check <- function(cf = 4, debug = F){
  throw <- sample(1:10,1) + sample(1:10,1)
  if(cf == 3) throw <- throw + 2
  if(cf == 6) throw <- throw - 2
  results <- c("Anger", "Sadness", "Fear", "Disfavors Thread", "Disfavors PC", "Focus NPC", "Favors NPC",
               "Focus PC", "Disfavors NPC", "Focus Thread", "Favors PC", "Favors Thread", "Courage",
               "Happiness", "Calm")
  if(throw < 4){
    result <- "Anger"
  }else if(throw > 18){
      result <- "Calm"
  }else{
      result <- results[throw-3]
    }
  if(debug){
    return(list(throw, result))
  }else{
      return(result)
    }
}

# GME Variations 2 Event Check ----
#' @export
gme_event_check <- function(){
  result <- c(rep("Remote Event", 7),
              rep("NPC Action", 21),
              rep("Introduce a new NPC", 7),
              rep("Move towards a thread", 10),
              rep("Move away from a thread", 7),
              rep("Close a thread", 3),
              rep("PC Negative", 12),
              rep("PC Positive", 8),
              rep("Ambiguous Event", 8),
              rep("NPC Negative", 9),
              rep("NPC Positive", 8))
  sample(result, 1)
}

# GME Meaning Tables: Descriptions ----
#' @export
gme_description_table <- function(){
  word1 <- c("Abnormally","Adventurously","Aggressively","Angrily","Anxiously","Awkwardly","Beautifully",
             "Bleakly","Boldly","Bravely","Busily","Calmly","Carefully","Carelessly","Cautiously","Ceaselessly",
             "Cheerfully","Combatively","Coolly","Crazily","Fully","Generously","Gently","Gladly","Gracefully",
             "Gratefully","Happily","Hastily","Healthily","Helpfully","Helplessly","Hopelessly","Innocently",
             "Intensely","Interestingly","Irritatingly","Jovially","Joyfully","Judgementally","Kindly",
             "Peacefully","Perfectly","Playfully","Politely","Positively","Powerfully","Quaintly","Quarrelsomely",
             "Quietly","Roughly","Rudely","Ruthlessly","Slowly","Softly","Swiftly","Threateningly","Very","Violently",
             "Wildly","Yieldingly","Curiously","Daintily","Dangerously","Defiantly","Deliberately","Delightfully",
             "Dimly","Efficiently","Energetically","Enormously","Enthusiastically","Excitedly","Fearfully",
             "Ferociously","Fiercely","Foolishly","Fortunately","Frantically","Freely","Frighteningly","Kookily",
             "Lazily","Lightly","Loosely","Loudly","Lovingly","Loyally","Majestically","Meaningfully","Mechanically",
             "Miserably","Mockingly","Mysteriously","Naturally","Neatly","Nicely","Oddly","Offensively","Officially",
             "Partially")
  word2 <- c("Abandoned","Abnormal","Amusing","Ancient","Aromatic","Average","Beautiful","Bizarre","Classy","Clean",
             "Cold","Colorful","Creepy","Cute","Damaged","Dark","Defeated","Delicate","Delightful","Dirty",
             "Graceful","Hard","Harsh","Healthy","Heavy","Historical","Horrible","Important","Interesting",
             "Juvenile","Lacking","Lame","Large","Lavish","Lean","Less","Lethal","Lonely","Lovely","Macabre",
             "Remarkable","Rotten","Rough","Ruined","Rustic","Scary","Simple","Small","Smelly","Smooth","Soft",
             "Strong","Tranquil","Ugly","Valuable","Warlike","Warm","Watery","Weak","Young","Disagreeable",
             "Disgusting","Drab","Dry","Dull","Empty","Enormous","Exotic","Faded","Familiar","Fancy","Fat",
             "Feeble","Feminine","Festive","Flawless","Fresh","Full","Glorious","Good","Magnificent","Masculine",
             "Mature","Messy","Mighty","Military","Modern","Extravagant","Mundane","Mysterious","Natural",
             "Nondescript","Odd","Pale","Petite","Poor","Powerful","Quaint","Rare","Reassuring")
  return(paste(sample(word1,1), sample(word2,1)))
}

# GME Meaning Tables: Actions ----
#' @export
gme_actions_table <- function(){
  word1 <- c("Attainment","Starting","Neglect","Fight","Recruit","Triumph","Violate","Oppose","Malice",
             "Communicate","Persecute","Increase","Decrease","Abandon","Gratify","Inquire","Antagonize",
             "Move","Waste","Truce","Expose","Haggle","Imprison","Release","Celebrate","Develop","Travel",
             "Block","Harm","Debase","Overindulge","Adjourn","Adversity","Kill","Disrupt","Usurp","Create",
             "Betray","Agree","Abuse","Excitement","Activity","Assist","Care","Negligence","Passion","Work",
             "Control","Attract","Failure","Pursue","Vengeance","Proceedings","Dispute","Punish","Guide",
             "Transform","Overthrow","Oppress","Change","Release","Befriend","Judge","Desert","Dominate",
             "Procrastinate","Praise","Separate","Take","Break","Heal","Delay","Stop","Lie","Return","Imitate",
             "Struggle","Inform","Bestow","Postpone","Oppress","Inspect","Ambush","Spy","Attach","Carry","Open",
             "Carelessness","Ruin","Extravagance","Trick","Arrive","Propose","Divide","Refuse","Mistrust",
             "Deceive","Cruelty","Intolerance","Trust")
  word2 <- c("Goals","Dreams","Environment","Outside","Inside","Reality","Allies","Enemies","Evil","Good",
             "Emotions","Opposition","War","Peace","Innocent","Love","Spirit","Intellect","Ideas","Joy",
             "Advice","Plot","Competition","Prison","Illness","Food","Attention","Success","Failure","Travel",
             "Jealousy","Dispute","Home","Investment","Suffering","Wishes","Tactics","Stalemate","Randomness",
             "Misfortune","Victory","Dispute","Riches","Normal","Technology","Hope","Magic","Illusions","Portals",
             "Danger","Weapons","Animals","Weather","Elements","Nature","Masses","Leadership","Fame","Anger",
             "Information","Messages","Energy","Balance","Tension","Friendship","Physical","Project","Pleasures",
             "Pain","Possessions","Benefits","Plans","Lies","Expectations","Legal","Bureaucracy","Business",
             "Path","News","Exterior","Death","Disruption","Power","Burden","Intrigues","Fears","Ambush","Rumor",
             "Wounds","Extravagance","Representative","Adversities","Opulence","Liberty","Military","Mundane",
             "Trials","Masses","Vehicle","Art")
  return(paste(sample(word1,1),sample(word2,1)))
}
