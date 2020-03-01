# Features ----

#' @export
iron_feature_aspect <- function(){
  res <- c("Blocked","Crafted","Ancient","Sunken","Trapped","Secret","Toxic","Ruined","Defended","Decaying","Marked","Guarded","Inaccessible","Foreboding","Veiled","Deep","Depleted","Foul","Elevated","Moving","Unnatural","Active","Confined","Fortified","Collapsed","Isolated","Destroyed","Open","Sacred","Flooded","Complex","Abundant","Hidden","Expansive","Mysterious","Unstable","Fragile","Broken","Ensnaring","Pillaged","Sealed","Makeshift","Treacherous","Natural","Dead","Unusual","Abandoned","Deadly","Forgotten","Mystical")
  sample(res, 1)
}

#' @export
iron_feature_focus <- function(){
  res <- c("Attack","Threshold","Boundary","Alarm","Exit","Passage","Crossing","Trigger","Trap","Hideaway","Nature","Sign","Refuge","Valuables","Breach","Route","Location","Trail","Supply","History","Prisoner","Habitation","Debris","Creature","Lair","Person","Enclosure","Remains","Water","Message","Darkness","Opening","Weapon","Entry","Illumination","Obstacle","Craft","Container","Information","Grave","Equipment","Shelter","Denizen","Environment","Material","Resource","Corruption","Death","Function","Power")
  sample(res,1)
}

#' @export
iron_feature <- function(){
  paste(iron_feature_aspect(), iron_feature_focus())
}

# NPC ----

#' @export
iron_npc_activity <- function(){
  res <- c("Guarding","Preserving","Constructing","Mending","Assisting","Securing","Learning","Sneaking","Fleeing","Sacrificing","Creating","Luring","Hunting","Seizing","Bargaining","Mimicking","Tricking","Tracking","Escorting","Hiding","Raiding","Socializing","Exploring","Journeying","Supporting","Avoiding","Disabling","Leading","Assaulting","Ensnaring","Defending","Recovering","Patrolling","Resting","Distracting","Leaving","Fighting","Ambushing","Controlling","Observing","Gathering","Suffering","Threatening","Searching","Destroying","Restoring","Consuming","Removing","Inspecting","Summoning")
  sample(res, 1)
}

# Combat ----

#' @export
iron_combat_method <- function(){
  res <- c("Defy","Break","Trick","Evade","Protect","Overwhelm","Persevere","Assist","Await","Abort","Block","Collide","Focus","Advance","Breach","Endure","Assault","Charge","Escalate","Sunder","Shatter","Aim","Stagger","Counter","Seize","Impact","Entangle","Hold","Deflect","Drop","Lose","Sweep","Secure","Cover","Withdraw","Clash","Amplify","Batter","Feint","Shove","Embed","Affect","Probe","Force","Intensify","Distract","Challenge","Brawl","Coordinate","Overrun")
  sample(res,1)
}

#' @export
iron_combat_target <- function(){
  res <- c("Control","Defense","Limbs","Focus","Advantage","Range","Stress","Sense","Weakness","Opening","Fear","Instinct","Footing","Maneuver","Reach","Harm","Finesse","Weapon","Environment","Technique","Surprise","Pride","Wound","Precision","Ally","Ground","Courage","Companion","Object","Momentum","Speed","Strength","Supply","Terrain","Armor","Skill","Body","Protection","Resolve","Ferocity","Shield","Ammo","Anger","Opportunity","Balance","Position","Barrier","Strategy","Grasp", "Power")
  sample(res,1)
}

#' @export
iron_combat <- function(){
  paste(iron_combat_method(),
        iron_combat_target())
}

# Trap ----

iron_trap_event <- function(){
 res <- c("Block","Create","Break","Puncture","Entangle","Enclose","Ambush","Snare","Change","Imitate","Crush","Drop","Conceal","Lure","Release","Obscure","Cut","Smother","Collapse","Summon","Move","Surprise","Divert","Attack","Trigger")
 sample(res,1)
}

iron_trap_component <- function(){
  res <- c("Pit","Water","Fire","Projectile","Passage","Fall","Debris","Fear","Alarm","Trigger","Cold","Weapon","Darkness","Decay","Path","Stone","Terrain","Poison","Barrier","Overhead","Magic","Toxin","Earth","Light","Denizen")
  sample(res, 1)
}

#' @export
iron_trap <- function(){
  paste(iron_trap_event(), iron_trap_component())
}
# Site ----

#' @export
iron_site_theme <- function(){
  sample(c("Ancient", "Corrupted", "Fortified", "Hallowed",
           "Haunted", "Infested", "Ravaged", "Wild"), 1)
}

#' @export
iron_site_domain <- function(){
  sample(c("Barrow", "Cavern", "Frozen Cavern", "Icereach",
           "Mine", "Pass", "Ruin", "Sea Cave", "Shadowfen",
           "Stronghold", "Tanglewood", "Underkeep"), 1)
}

#' @export
iron_site <- function(){
  paste(iron_site_theme(), iron_site_domain())
}

# Site name ----

#' @export
iron_site_name <- function(domain = iron_site_domain()){
  roll <- sample(1:100, 1)
  rolls <- c(25, 50, 70, 80, 85, 95, 100)
  position <-which(roll <= rolls)[1]
  description <- iron_site_description()
  place <- iron_site_place(domain)
  detail <- iron_site_detail()

  if(position == 1){
    return(paste(description,
                 place))
  }
  if(position == 2){
    return(paste(place,
                 "of",
                 detail))
  }
  if(position == 3){
    return(paste(place,
                 "of",
                 description,
                 detail))
  }
  if(position == 4){
    return(paste("(Name's)",
                 place))
  }
  if(position == 5){
    return(paste(description,
                 place,
                 "of (name)"))
  }
  if(position == 6){
    return(paste(place,
                 "of (name)"))
  }
}

iron_site_description <- function(){
  res <- c("Deep","Tainted","Grey","Forgotten","Flooded","Forbidden","Barren","Lost","Cursed","Fell","Sunken","Nightmare","Infernal","Dark","Bloodstained","Haunted","White","Shrouded","Wasted","Grim","Endless","Crumbling","Undying","Bloodied","Forsaken","Silent","Blighted","Iron","Frozen","Abyssal","Crimson","Silver","Desecrated","Ashen","Elder","Scorched","Unknown","Scarred","Broken","Chaotic","Black","Hidden","Sundered","Shattered","Dreaded","Secret","High","Sacred","Fallen","Ruined")
  sample(res,1)
}

iron_site_detail <- function(){
  res <- c("Blight","Strife","Nightfall","Fury","Terror","Truth","Spring","Sanctuary","Bone","Specters","Daybreak","Doom","Treachery","Blood","War","Torment","Iron","Silence","Mist","Isolation","Runes","Rot","Corruption","Prophecy","Fate","Twilight","Power","Darkness","Gloom","Storms","Hope","Lament","Frost","Souls","Winter","Sadness","Desolation","Bane","Lies","Ash","Banishment","Shadow","Madness","Stone","Secrets","Despair","Blades","Dread","Light","Wrath")
  sample(res, 1)
}

#' @export
iron_site_place <- function(domain = iron_site_domain()){
  if(domain == "Barrow"){
    return(sample(c("Sepulcher",
                    "Grave",
                    "Crypt",
                    "Mound",
                    "Tomb",
                    "Barrow"),
                  1))
  }
  if(domain == "Cavern" | domain == "Frozen Cavern"){
    return(sample(c("Abyss",
                    "Caverns",
                    "Caves",
                    "Chasm",
                    "Depths",
                    "Hollow",
                    "Lair",
                    "Rift",
                    "Tunnels",
                    "Warren"),
                  1))
  }
  if(domain == "Icereach"){
    return(sample(c("Icemark",
                    "Wintertide",
                    "Reach",
                    "Waste",
                    "Expanse",
                    "Barrens"),
                  1))
  }
  if(domain == "Mine"){
    return(sample(c("Lode",
                    "Dig",
                    "Forge",
                    "Mine",
                    "Tunnels",
                    "Cut"),
                  1))
  }
  if(domain == "Pass"){
    return(sample(c("Cliffs",
                    "Crag",
                    "Cut",
                    "Gap",
                    "Gorge",
                    "Heights",
                    "Highlands",
                    "Pass",
                    "Reach",
                    "Ridge"),
                  1))
  }
  if(domain == "Ruin"){
    return(sample(c("Citadel",
                    "Enclave",
                    "Fortress",
                    "Hall",
                    "Keep",
                    "Sanctuary",
                    "Sanctum",
                    "Spire",
                    "Temple",
                    "Tower"),
                  1))
  }
  if(domain == "Sea Cave"){
    return(sample(c("Caves",
                    "Channel",
                    "Cove",
                    "Hollow",
                    "Pools",
                    "Gouge"),
                  1))
  }
  if(domain == "Shadowfen"){
    return(sample(c("Bog",
                    "Fen",
                    "Lowland",
                    "Marsh",
                    "Mire",
                    "Morass",
                    "Quagmire",
                    "Floodlands",
                    "Slough",
                    "Wetlands"),
                  1))
  }
  if(domain == "Stronghold"){
    return(sample(c("Bastion",
                    "Citadel",
                    "Fortress",
                    "Garrison",
                    "Haven",
                    "Keep",
                    "Outpost",
                    "Refuge",
                    "Sanctuary",
                    "Watch"),
                  1))
  }
  if(domain == "Tanglewood"){
    return(sample(c("Weald",
                    "Tangle",
                    "Bramble",
                    "Briar",
                    "Thicket",
                    "Forest",
                    "Wilds",
                    "Wood"),
                  1))
  }
  if(domain == "Underkeep"){
    return(sample(c("Catacombs",
                    "Chambers",
                    "Den",
                    "Hall",
                    "Labyrinth",
                    "Maze",
                    "Pit",
                    "Sanctum",
                    "Underkeep",
                    "Vault"),
                  1))
  }
}
