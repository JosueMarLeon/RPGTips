#' @export
gma_base_prompt <- function(){
  tools::toTitleCase(paste(sample(gma_base_verbs, 1),
                    sample(gma_base_adjectives, 1),
                    sample(gma_base_names, 1)))
}