context("Distributions")
library(cookbook)

test_that("Right sum of Pascals diagonal is given.", {
  expect_equal(sum_Pascal_diagonal(7, 0), 8)
  expect_equal(sum_Pascal_diagonal(7, 2), 56)
  expect_equal(sum_Pascal_diagonal(20, 3), 5985)
  expect_equal(sum_Pascal_diagonal(20, 4), 20349)
  #expect_equal(sum_Pascal_diagonal(0, 1), 1)
  #expect_equal(sum_Pascal_diagonal(3, 4), 1)
})
