source("data-raw/gme_plot_points_themes.R")
source("data-raw/gma_base_prompts.R")
usethis::use_data(plot_points,
                  gma_base_adjectives,
                  gma_base_names,
                  gma_base_verbs,
                  internal = T,
                  overwrite = T)
