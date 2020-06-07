# Generators related to magic ----

#' @export
magic_tradition <- function(){
  r <- c("Air","Alchemy","Alteration","Arcana","Battle","Blood","Celestial","Chaos","Conjuration","Curse",
         "Death","Demonology","Destruction","Divination","Earth","Enchantment","Exorcism","Fey","Fire",
         "Forbidden","Illusion","Invocation","Life","Metal","Nature","Necromancy","Order","Primal",
         "Protection","Rune","Shadow","Shaman","Song","Soul","Spiritualism","Storm","Technomancy","Telekinesis",
         "Telepathy","Teleportation","Templar","Theurgy","Time","Transformation","Water","Witch")
  return(sample(r,1))
}

#' @export
magic_spell <- function(flashy = rbinom(1, 1, 0.75)){
  base_spell <- maze_magic()

  addendum_type <- sample(c('master',
                            'group'),
                          size = 1)

  addendum <- tools::toTitleCase(switch(addendum_type,
                                        'master' = silent_cult_complex_master(),
                                        'group' = silent_cult_complex_group()))

  ifelse(flashy,
         yes = ifelse(test = rbinom(1, 1, 0.5) == 1,
                      yes = paste(base_spell, 'of the', addendum),
                      no = paste0(addendum, '\'s ', base_spell)),
         no = base_spell)

}