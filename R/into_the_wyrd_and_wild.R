#' @export
wyrd_danger <- function(){sample(wyrd_danger_l,1)}

#' @export
wyrd_dungeon <- function(){paste(sample(wyrd_dungeon_prefix_l,1),
                                 sample(wyrd_dungeon_suffix_l,1))}

#' @export
wyrd_random_path <- function(){sample(wyrd_random_paths_l, 1)}

#' @export
wyrd_smell_sound <- function(){sample(wyrd_smells_sounds_l, 1)}

#' @export
wyrd_treasure <- function(){sample(wyrd_treasure_l, 1)}

#' @export
wyrd_wilderness_scene <- function(){sample(wyrd_wilderness_scenes_l, 1)}

#' @export
wyrd_woods_type <- function(){sample(wyrd_woods_types_l, 1)}

#' @export
wyrd_monster <- function(){sample(wyrd_monster_l, 1)}

#' @export
wyrd_complete_dungeon <- function(){
  paste("The ",
        wyrd_dungeon(),
        ", where ",
        gsub(pattern = ".", replacement = "", x = tolower(wyrd_danger()), fixed = T),
        ", but you can find ",
        tolower(wyrd_treasure()),
        sep = "")
}