silent_cult_group <- function(){
  sample(c('academy',
           'brotherhood',
           'chantry',
           'church',
           'circle',
           'compact',
           'creed',
           'fellowship',
           'pact',
           'rite',
           'society',
           'sodality'),
         size = 1)
}

silent_cult_occult <- function(){
  sample(c('alchemic',
           'arcane',
           'cabalistic',
           'goetic',
           'hermetic',
           'hidden',
           'occult',
           'primitive',
           'sorcerous',
           'theurgic',
           'transcendent',
           'veritable'),
         size = 1)
}

silent_cult_hue <- function(){
  sample(c('argent',
           'azure',
           'black',
           'crimson',
           'fuligin',
           'golden',
           'gray',
           'illuminated',
           'ivory',
           'shadow',
           'veiled',
           'viridian'),
         size = 1)
}

silent_cult_object <- function(){
  sample(c('book',
           'chalice',
           'crown',
           'eye',
           'fist',
           'hand',
           'message',
           'sword',
           'throne',
           'treasure',
           'truth',
           'vision'),
         size = 1)
}

silent_cult_count <- function(){
  sample(c('countless',
           'dual',
           'legion',
           'manifold',
           'myriad',
           'one',
           'primary',
           'singular',
           'sole',
           'solitary',
           'threefold',
           'unified'),
         size = 1)
}

silent_cult_laud <- function(){
  sample(c('adept',
           'ancient',
           'ascetic',
           'austere',
           'benevolent',
           'blessed',
           'enlightened',
           'holy',
           'learned',
           'noble',
           'sagacious',
           'venerable'),
         size = 1)
}

silent_cult_material <- function(){
  sample(c('blood',
           'bone',
           'bronze',
           'copper',
           'diamond',
           'golden',
           'iron',
           'leaden',
           'mercurial',
           'ruby',
           'sapphire',
           'skin'),
         size = 1)
}

silent_cult_master <- function(){
  sample(c('demiurge',
           'god',
           'king',
           'lady',
           'lord',
           'master',
           'mistress',
           'prince',
           'prophet',
           'queen',
           'sage',
           'star'),
         size = 1)
}

silent_cult_complex_master <- function(){
  switch(sample(1:9,size = 1),
         '1' = silent_cult_master(),
         '2' = paste(silent_cult_hue(),
                     silent_cult_master()),
         '3' = paste(silent_cult_laud(),
                     silent_cult_master()),
         '4' = paste(silent_cult_material(),
                     silent_cult_master()),
         '5' = paste(silent_cult_occult(),
                     silent_cult_master()),
         '6' = paste(silent_cult_object(),
                     silent_cult_master()),
         '7' = silent_god(),
         '8' = silent_god_epithet(),
         '9' = paste(silent_god_adjective(),
                     silent_cult_master()))
}

silent_cult_complex_group <- function(){
  switch(sample(1:6, size = 1),
         '1' = paste(silent_cult_occult(),
                     silent_cult_group()),
         '2' = paste(silent_cult_hue(),
                     silent_cult_group()),
         '3' = paste(silent_cult_laud(),
                     silent_cult_group()),
         '4' = paste(silent_cult_material(),
                     silent_cult_group()),
         '5' = paste0(silent_cult_complex_master(), '\'s ',
                      silent_cult_group()),
         '6' = paste(silent_cult_hue(),
                     silent_cult_object(),
                     silent_cult_group()))
}

silent_god_epithet <- function(){
  switch(sample(1:4,
                size = 1),
         '1' = sample(c('conceiver',
                        'creator',
                        'deviser',
                        'father',
                        'forge',
                        'incarnator',
                        'maker',
                        'mother',
                        'source',
                        'womb'), size = 1),
         '2' = sample(c('breaker',
                        'destroyer',
                        'devourer',
                        'eater',
                        'eradicator',
                        'feaster',
                        'ravener',
                        'reviler',
                        'ruiner',
                        'thirster'), size = 1),
         '3' = sample(c('autarch',
                        'hierarch',
                        'king',
                        'lord',
                        'master',
                        'pontifex',
                        'prince',
                        'ruler',
                        'sultan',
                        'tyrant'), size = 1),
         '4' = sample(c('beholder',
                        'discerner',
                        'diviner',
                        'haruspex',
                        'knower',
                        'oracle',
                        'prophet',
                        'seer',
                        'speaker',
                        'whisperer'), size = 1))
}

silent_god_adjective <- function(){
  sample(c('alabaster','anguish','arm','ashen','bleeding','blue','breeding',
           'burrowing','claw','consuming','craving','crimson','dancing','dawn',
           'day','descending','dreaming','dusk','emerald','empty','eye',
           'festering','first','fivefold','fourfold','fuligin','greed',
           'growing','hand','hatred','head','howling','hunger','incarnadine',
           'indifference','listening','living','lust','many','memory','mouth',
           'night','opalescent','purple','radiant','rage','ravening','red',
           'rotting','sagacious','sand','scarlet','scron','screaming','second',
           'sevenfold','shrieking','silken','singing','sixfold','sleeping',
           'smoke','sorrow','stinking','tendril','third','thirsting','thorns',
           'tone','undying','unknowable','unseeing','viridian','wailing',
           'waiting','walking','watching','wing','writhing','yellow'),
         size = 1)
}

#' @export
silent_god <- function(){
  tools::toTitleCase(paste(silent_god_adjective(),
                           silent_god_epithet()))
}

#' @export
silent_cult <- function(){
  res <- switch(sample(1:12, size = 1),
                '1' = paste(silent_cult_group(),
                            'of The',
                            silent_cult_hue(),
                            silent_cult_object()),
                '2' = paste(silent_cult_object(),
                            'of The',
                            silent_cult_laud(),
                            silent_cult_master()),
                '3' = paste(silent_cult_hue(),
                            silent_cult_group(),
                            'of',
                            silent_cult_occult(),
                            silent_cult_count()),
                '4' = paste(silent_cult_laud(),
                            silent_cult_group(),
                            'of The',
                            silent_cult_count(),
                            silent_cult_master()),
                '5' = paste(silent_cult_occult(),
                            silent_cult_object(),
                            'of The',
                            silent_cult_material(),
                            silent_cult_master()),
                '6' = paste(silent_cult_material(),
                            silent_cult_object(),
                            'of The',
                            silent_cult_hue(),
                            silent_cult_group()),
                '7' = paste(silent_cult_count(),
                            silent_cult_group(),
                            'of The',
                            silent_cult_occult(),
                            silent_cult_master()),
                '8' = paste(silent_cult_hue(),
                            silent_cult_object(),
                            'of The',
                            silent_cult_material(),
                            silent_cult_master()),
                '9' = paste0(silent_cult_hue(),' ',
                            silent_cult_object(),
                            ' of The ',
                            silent_cult_master(),'\'s ',
                            silent_cult_group()),
                '10' = paste(silent_cult_hue(),
                            silent_cult_count(),
                            'of The',
                            silent_cult_occult(),
                            silent_cult_master()),
                '11' = paste0(silent_cult_laud(),' ',
                            silent_cult_master(),'\'s ',
                            silent_cult_group()),
                '12' = paste(silent_cult_material(),
                            silent_cult_group(),
                            'of The',
                            silent_cult_laud(),
                            silent_cult_master()))
  tools::toTitleCase(res)
}