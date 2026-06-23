
library(testthat)

test_that("auc trapezoid works for simple case", {
  x <- c(1, 2, 3)
  y <- c(0, 1, 0)
  
  res <- auc(x, y)
  expect_equal(res, 1)
})

test_that("auc step method works", {
  x <- c(1, 2, 3)
  y <- c(0, 1, 0)
  
  res <- auc(x, y, method = "step")
  expect_equal(res, 1)
})

test_that("auc spline runs without error", {
  x <- c(1, 2, 3, 4)
  y <- c(0, 1, 0, 1)
  
  res <- auc(x, y, method = "spline")
  expect_true(is.numeric(res))
})

test_that("auc handles unsorted x", {
  x <- c(3, 1, 2)
  y <- c(0, 0, 1)
  
  res <- auc(x, y)
  expect_true(is.numeric(res))
})

test_that("auc absoluteArea works", {
  x <- c(1, 2, 3)
  y <- c(-1, 1, -1)
  
  res <- auc(x, y, absoluteArea = TRUE)
  expect_true(res > 0)
})

test_that("auc respects from/to limits", {
  x <- 1:5
  y <- c(0, 1, 1, 1, 0)
  
  full <- auc(x, y)
  partial <- auc(x, y, from = 2, to = 4)
  
  expect_true(partial < full)
})

test_that("auc removes NA when na.rm = TRUE", {
  x <- c(1, 2, 3)
  y <- c(0, NA, 1)
  
  res <- auc(x, y, na.rm = TRUE)
  expect_true(is.numeric(res))
})

test_that("auc returns NA for too short input", {
  expect_true(is.na(auc(1, 1)))
})

test_that("auc errors on unequal length", {
  expect_error(auc(1:3, 1:2))
})

