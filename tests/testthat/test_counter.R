context("Counter")
library(cookbook)

testthat::test_that("the counter works", {
  testthat::expect_equal(counter(c(1,1,1), 3), c(1,1,2))
  testthat::expect_equal(counter(c(1,1,3), 3), c(1,2,1))
  testthat::expect_equal(counter(c(1,3,3), 3), c(2,1,1))
  testthat::expect_equal(counter(c(5,8,8,8), 8), c(6,1,1,1))
  testthat::expect_equal(counter(c(1,5,5), 5), c(2,1,1))
  testthat::expect_equal(counter(c(1), 5), 2)
  testthat::expect_equal(counter(c(3), 3), 1)
})
