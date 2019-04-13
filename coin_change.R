# How many different ways of giving chance given a vector with coins?

# Solution 1, recursive
coin_change <- function(coins, change, m){
  if(change < 0 || m <= 0){
    return(0)
  }
  if(change == 0){
    return(1)
  }
  return(coin_change(coins, change, m - 1) + coin_change(coins, change - coins[m], m))
}

# Solution 2, brute force
coin_change2 <- function(coins, change) {
  max_count <- floor(change / coins)
  valid <- which(max_count != 0)
  max_count <- max_count[valid]; coins <- coins[valid]
  coefficients <- list()
  for(i in 1:length(coins)){
    coefficients[[i]] <- 0:max_count[i]
  }
  combis <- expand.grid(coefficients)
  sum <- apply(combis, MARGIN = 1, FUN = function(X){sum(X*coins)})
  return(length(which(sum == change)))
}
