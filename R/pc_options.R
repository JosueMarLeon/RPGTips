#' @export
pc_background <- function(system = "5e", complete = FALSE){
  backs <- switch(system,
                  "5e" = backgrounds_5e,
                  "p2e" = backgrounds_p2e)
  num <- sample(1:nrow(backs), 1)
  if(complete){
    return(backs[num,])
  }else{
    return(backs[num,1])
  }
}