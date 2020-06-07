# n = 123, returns 1+2+3=6
digital_root <- function(n){
  result <- sum(as.numeric(strsplit(as.character(n), "")[[1]]))
  if(nchar(result) == 1){
    return(result)
  }else{
    return(digital_root(result))
  }
}

# n = 3, k = 0**2, 1**2, 2**2, 3**2
# count all digits d
nbDig <- function(n, d) {
  k <- as.character(format((0:n)^2), scientific = FALSE)
  indices <- unlist(gregexpr(as.character(d), k))
  return(length(indices[indices > 0]))
}

