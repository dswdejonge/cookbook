# Refactored
letter_value <- function(l){
  which(tolower(l) == letters)
}

letter_vector <- function(s){
  strsplit(s,"")[[1]]
}

letter_vector_sum <- function(v){
  sum(sapply(tolower(v), FUN = letter_value))
}

# Combinations
words_to_marks <- function(s){
  letter_vector_sum(letter_vector(s))
}

add_letters <- function(v) {
  if(length(v) == 0){return("z")}
  i <- letter_vector_sum(v) %% 26
  return(letters[ifelse(i == 0, 26, i)])
}
