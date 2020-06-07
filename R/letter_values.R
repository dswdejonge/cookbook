words_to_marks <- function(s){
  sum(sapply(strsplit(s,"")[[1]], FUN = function(x){which(x == letters)}))
}
