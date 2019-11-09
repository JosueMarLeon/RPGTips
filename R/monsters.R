#' @export
monster_osr <- function(min_hd = 1, max_hd = 99){
  res <- monsters_osr[monsters_osr$HD >= min_hd,]
  res <- res[res$HD <= max_hd,]
  res[sample(1:nrow(res),1),c(1,2,3,5,6,7,8,9,11,13)]
}

#' @export
monster_path <- function(min_cr = 0, max_cr = 40){
  res <- monsters_path[monsters_path$CR >= min_cr,]
  res <- res[res$CR <= max_cr,]
  res[sample(1:nrow(res),1),c(1,2,9,10,11,12,18,21,22,71)]
}