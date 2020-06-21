# Refactored
letter_values <- function(v){
  match(tolower(v),letters)
}

letter_vector <- function(s){
  strsplit(s,"")[[1]]
}

# Combinations (Code Wars)
words_to_marks <- function(s){
  sum(letter_values(letter_vector(s)))
}

add_letters <- function(v) {
  i <- sum(letter_values(v)) %% 26
  return(letters[ifelse(i == 0, 26, i)])
}

# Pangram = a sentence that contains all letters in the alphabet.
is_pangram <- function(s){
  all(letters %in% tolower(letter_vector(s)))
}
