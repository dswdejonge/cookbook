source("coin_change.R")
# Expected 3
coin_change(
  coins = c(1,2),
  change = 4)
# Expected 1022
coin_change(
  coins = c(5,10,20,50,100,200,500),
  change = 300)
