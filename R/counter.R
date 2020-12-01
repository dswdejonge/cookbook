# v = counter vector
# n = max value for each number
# i = index in counter
# TODO: begin by 0
# TODO: +1 not hardwired
# TODO: different n per integer
counter <- function(v, n, i = length(v)){
  v[i] <- v[i]+1
  if(v[i] > n){
    v[i] <- 1
    return(counter(v, n, i-1))
  }else{
    return(v)
  }
}


