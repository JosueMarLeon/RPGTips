maze_magic_physical_effect <- function(){
  sample(c('animating','attracting','binding','blossoming','consuming',
           'creeping','crushing','diminishing','dividing','duplicating',
           'enveloping','expanding','fusing','grasping','hastening',
           'hindering','illuminating','imprisoning','levitating',
           'opening','petrifying','phasing','piercing','pursuing','reflecting',
           'regenerating','rending','repelling','resurrecting','screaming',
           'sealing','shapeshifting','shielding','spawning','transmuting',
           'transporting'),
         size = 1)
}

maze_magic_physical_elements <- function(){
  sample(c('acid','amber','bark','blood','bone','brine','clay','crow','crystal','ember',
  'flesh','fungus','glass','honey','ice','insect','wood','lava','moss',
  'obsidian','oil','poison','rat','salt','sand','sap','serpent','slime',
  'stone','tar','thorn','vine','water','wine','wood','worm','dragon','demon',
  'eldritch beast', 'cataclysm'), size = 1)
}

maze_magic_physical_forms <- function(){
  sample(c('altar','armor','arrow','beast','blade','cauldron','horn','key',
           'mask','monolith','pit','prison','chain','chariot','claw','cloak',
           'colossus','crown','sentinel','servant','shield','spear','steed',
           'swarm','elemental','eye','fountain','gate','golem','hammer',
           'tentacle','throne','torch','trap','wall','web'), size = 1)
}

maze_magic_ethereal_effect <- function(){
  sample(c('avenging','banishing','bewildering','blinding','charming',
           'communicating','compelling','concealing','deafening','deceiving',
           'deciphering','disguising','dispelling','embodelning','encoding',
           'energizing','enlightening','enraging','excruciating','foreseeing',
           'intoxicating','maddening','mesmerizing','mindreading','nullifying',
           'paralyzing','revealing','revolting','scrying','silencing',
           'soothing','summoning','terrifying','warding','wearying','withering',
           'searing'), size = 1)
}

maze_magic_ethereal_elements <- function(){
  sample(c('ash','chaos','distortion','dream','dust','echo','ectoplasm','fire',
           'fog','ghost','harmony','heat','light','lightning','memory','mind',
           'mutation','negation','plague','plasma','probability','rain','rot',
           'shadow','pain','smoke','snow','soul','star','stasis','steam',
           'thunder','time','void','warp','whisper','wind','convocation'),
         size = 1)
}

maze_magic_ethereal_forms <- function(){
  sample(c('aura','beacon','beam','blast','blob','bolt','gaze','loop','moment',
           'nexus','portal','pulse','bubble','call','cascade','circle','cloud',
           'coil','pyramid','ray','shard','sphere','spray','storm','cone','cube',
           'dance','disk','field','form','swarm','torrent','touch','vortex',
           'wave','word'),
         size = 1)
}


#' @export
maze_magic <- function(){
  roll1 <- sample(1:6, size = 1)
  roll2 <- sample(1:6, size = 1)
  if(roll2 >= 4){
    res <- switch(roll1,
                  '1' = paste(maze_magic_physical_effect(),
                            maze_magic_physical_forms()),
                  '2' = paste(maze_magic_physical_effect(),
                            maze_magic_ethereal_forms()),
                  '3' = paste(maze_magic_ethereal_effect(),
                            maze_magic_physical_forms()),
                  '4' = paste(maze_magic_ethereal_effect(),
                            maze_magic_ethereal_forms()),
                  '5' = paste(maze_magic_physical_elements(),
                            maze_magic_physical_forms()),
                  '6' = paste(maze_magic_physical_elements(),
                            maze_magic_ethereal_forms()))
  }else{
    res <- switch(roll1,
                  '1' = paste(maze_magic_ethereal_elements(),
                            maze_magic_physical_forms()),
                  '2' = paste(maze_magic_ethereal_elements(),
                            maze_magic_ethereal_forms()),
                  '3' = paste(maze_magic_physical_effect(),
                            maze_magic_physical_elements()),
                  '4' = paste(maze_magic_physical_effect(),
                            maze_magic_ethereal_elements()),
                  '5' = paste(maze_magic_ethereal_effect(),
                            maze_magic_physical_elements()),
                  '6' = paste(maze_magic_ethereal_effect(),
                            maze_magic_ethereal_elements()))
  }
  tools::toTitleCase(res)
}


