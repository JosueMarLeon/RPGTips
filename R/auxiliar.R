#' @export
roll_table <- function(fun){
  f <- fun
  set.seed(seed_counter)
  r <- f()
  seed_counter <<- seed_counter + 1
  return(r)
}

#' @export
roll <- function(number = 1, dice = 6, type = "normal", bonus = 0){
  if(type =="fate"){
    rolls <- sample(x = c(1,0,-1), size = 4, replace = T)
    return(sum(rolls)+bonus)
  }
  rolls <- sample(x = 1:dice, size = number, replace = T)
  if(type == "normal"){
    return(sum(rolls)+bonus)
  }
  if(type == "drop_min"){
    return(sum(rolls[rolls > min(rolls)])+bonus)
  }
  if(type == "advantage"){
    return(max(rolls)+bonus)
  }
  if(type == "disadvantage"){
    return(min(rolls)+bonus)
  }
}

#' @export
list_tools <- function(){
  funs <- ls("package:RPGTips")
  funs <- funs[!grepl(pattern = "roll", x = funs)]
  splitted <- strsplit(x = funs, split = "_")
  prefixes <- character()
  for(i in 1:length(splitted)){
    prefixes[i] <- splitted[[i]][1]
  }
  for(i in 1:length(unique(prefixes))){
    cat(unique(prefixes)[i])
    cat("\n")
  }
}
