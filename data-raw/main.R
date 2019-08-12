source("data-raw/gme_plot_points_themes.R")
source("data-raw/gma_base_prompts.R")
source("data-raw/backgrounds5e.R")
source("data-raw/backgroundsp2e.R")
source("data-raw/wyrd_wild_files.R")
usethis::use_data(plot_points,
                  gma_base_adjectives,
                  gma_base_names,
                  gma_base_verbs,
                  backgrounds_5e,
                  backgrounds_p2e,
                  wyrd_dungeon_prefix_l,
                  wyrd_dungeon_suffix_l,
                  wyrd_random_paths_l,
                  wyrd_smells_sounds_l,
                  wyrd_danger_l,
                  wyrd_treasure_l,
                  wyrd_wilderness_scenes_l,
                  wyrd_woods_types_l,
                  wyrd_monster_l,
                  internal = T,
                  overwrite = T)
