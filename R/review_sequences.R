find_missing <- function(sequence){
  diffs <- diff(sequence)
  off <- which(!duplicated(diffs) & !duplicated(diffs, fromLast = T))
  progression <- unique(diffs[-(off)])
  missing_value <- sequence[off]+progression
  return(missing_value)
}

test_that("Example Test", {
  expect_equal(find_missing(c(1, 2, 3, 4, 6, 7, 8, 9)), 5)
  expect_equal(find_missing(c(1, 3, 4, 5, 6, 7, 8, 9)), 2)
})
