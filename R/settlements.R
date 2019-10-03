# Functions to create a random settlement
# Sources:
#  Settlement rules, PFRPG
#  Empire Builder series, Ennead Games

#' @export
settlement_size <- function(){
  sizes <- c("Thorp (<21)",
             "Hamlet (21-60)",
             "Village (61-200)",
             "Small town (201-2000)",
             "Large town (2001-5000)",
             "Small city (5001-10000)",
             "Large city (10001-25000)",
             "Metropolis (>25000)")
 res <- min(sample(1:8, size = 2, replace = T))
 sizes[res]
}

#' @export
settlement_name <- function(){
  option <- as.logical(rbinom(n = 1, size = 1, prob = 0.5))
  prefix <- c("Castle", "Cape", "Deep", "East", "Fort",
              "Grand", "Greater", "High", "Higher", "Inner",
              "Lake", "Lesser", "Long", "Low", "Lower",
              "Middle", "Mount", "New", "North", "Old",
              "Outer", "Port", "South", "Upper", "West")
  res <- paste0(sample(settlement_names_main, 1),
                sample(settlement_names_suffix, 1))
  ifelse(option, yes = paste(sample(prefix, 1), res), no = res)
}

#' @export
settlement_government <- function(){
govs_5e <- c("Autocracy","Bureaucracy","Confederacy","Democracy","Dictatorship",
             "Feudalism","Gerontocracy","Hierarchy","Magocracy","Matriarchy",
             "Militocracy","Monarchy","Oligarchy","Patriarchy","Meritocracy",
             "Plutocracy","Republic","Satrapy", "Kleptocracy","Theocracy")
sample(govs_5e, 1)
}