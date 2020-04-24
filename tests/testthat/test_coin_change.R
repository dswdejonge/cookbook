context("Coin change")
library(cookbook)

test_that("Coordinates are transformed correctly", {
  expect_equal(coin_change2(coins = c(1,2),change = 4),3)
  expect_equal(coin_change(coins = c(5,10,20,50,100,200,500),change = 300,m = 7),1022)
  expect_equal(coin_change2(coins = c(1,2,5),change = 7),6)
  expect_equal(coin_change(coins = c(5,10),change = 300,m = 2),31)
})


