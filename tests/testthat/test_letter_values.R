context("Letter values")
library(cookbook)

test_that("The sum of words are correct", {
  expect_equal(words_to_marks("abc"),6)
  expect_equal(words_to_marks("ABC"),6)
  expect_equal(words_to_marks("AbC"),6)
})

test_that('add_letters works', {
  expect_equal(add_letters(c('a', 'b', 'c')), 'f')
  expect_equal(add_letters(c('z')), 'z')
  expect_equal(add_letters(c('a', 'b')), 'c')
  expect_equal(add_letters(c('c')), 'c')
  expect_equal(add_letters(c('z', 'a')), 'a')
  expect_equal(add_letters(c('y', 'c', 'b')), 'd')
  expect_equal(add_letters(c()), 'z')
})

test_that("is_Pangram works", {
  pangram <- "The quick, brown fox jumps over the lazy dog!"
  expect_equal(is_pangram(pangram), TRUE)
  expect_equal(is_pangram("This is not."), FALSE)
})
