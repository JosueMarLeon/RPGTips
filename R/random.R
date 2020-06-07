#' @export
rand_simple_oracle <- function(){
  return(sample(c("Yes, and",
                  "Yes",
                  "Yes, but",
                  "No, but",
                  "No",
                  "No, and"),
                1))
}


#' @export
rand_recluse <- function(pos = 1, neg = 1){
  white <- max(sample(1:6, pos, replace = T))
  black <- max(sample(1:6, neg, replace = T))
  if(white == black) return('Some presupposition behind the question is wrong!')
  if(white > black) res <- 'Yes'
  if(white < black) res <- 'No'
  if(white <=3 & black <= 3) res <- paste(res, ', but', sep = '')
  if(white >=4 & black >= 4) res <- paste(res, ', and', sep = '')
  return(res)
}

#' @export
rand_investigation_question <- function(){
  res <-  c("Yes, and a clue proves the entire questioned theory to be true.",
  "Yes. The answer is merely affirmative.",
  "Yes, but a clue proves some part of the questioned theory to be false.",
  "No, but a clue proves some part of the questioned theory to be true.",
  "No. The answer is merely negative.",
  "No, and a clue proves the entire questioned theory to be false.)")
  sample(res, 1)
}