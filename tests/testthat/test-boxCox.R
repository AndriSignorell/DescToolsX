
library(testthat)

## --- basic functionality ---

test_that("boxCox returns numeric vector of same length", {
  x <- runif(100, 1, 10)
  y <- boxCox(x, lambda = 0.5)
  
  expect_type(y, "double")
  expect_equal(length(y), length(x))
})

## --- inverse correctness ---

test_that("boxCox and inverse recover original values", {
  set.seed(1)
  x <- runif(100, 1, 10)
  
  for (lambda in c(-1, -0.5, 0, 0.5, 1)) {
    y <- boxCox(x, lambda)
    x2 <- boxCoxInv(y, lambda)
    
    expect_equal(x, x2, tolerance = 1e-8)
  }
})

## --- lambda approx zero (log case) ---

test_that("lambda near zero uses log transform", {
  x <- runif(100, 1, 10)
  
  y1 <- boxCox(x, lambda = 0)
  y2 <- boxCox(x, lambda = 1e-10)
  
  expect_equal(y1, log(x))
  expect_equal(y2, log(x), tolerance = 1e-8)
})

## --- invalid input: non-positive values ---

test_that("errors on non-positive values", {
  x <- c(1, 2, 0, 3)
  
  expect_error(
    boxCox(x, lambda = 0.5),
    "strictly positive"
  )
})

## --- invalid lambda ---

test_that("invalid lambda throws error", {
  x <- runif(10, 1, 10)
  
  expect_error(boxCox(x, lambda = c(1,2)))
  expect_error(boxCox(x, lambda = NA))
  expect_error(boxCox(x, lambda = Inf))
})

## --- NA handling ---

test_that("NA values are preserved", {
  x <- c(1, 2, NA, 4)
  
  y <- boxCox(x, lambda = 0.5)
  
  expect_true(is.na(y[3]))
})

## --- all NA error ---

test_that("all NA input throws error", {
  x <- c(NA, NA)
  
  expect_error(
    boxCox(x, lambda = 0.5),
    "only NA"
  )
})

## --- empty input ---

test_that("empty input returns empty output", {
  x <- numeric(0)
  
  y <- boxCox(x, lambda = 0.5)
  
  expect_equal(y, numeric(0))
})

## --- inverse domain error ---

test_that("inverse errors when lambda*x+1 <= 0", {
  x <- c(-10, -5)
  lambda <- 0.5
  
  expect_error(
    boxCoxInv(x, lambda),
    "must be positive"
  )
})

## --- inverse NA handling ---

test_that("inverse preserves NA", {
  x <- c(1, NA, 3)
  
  y <- boxCoxInv(x, lambda = 0.5)
  
  expect_true(is.na(y[2]))
})

## --- inverse all NA ---

test_that("inverse errors on all NA", {
  x <- c(NA, NA)
  
  expect_error(
    boxCoxInv(x, lambda = 0.5),
    "only NA"
  )
})

## --- comparison with forecast package (optional) ---

test_that("matches forecast::BoxCox", {
  skip_if_not_installed("forecast")
  
  library(forecast)
  
  set.seed(1)
  x <- runif(100, 1, 10)
  
  y1 <- boxCox(x, lambda = 0.5)
  y2 <- BoxCox(x, lambda = 0.5)
  
  expect_equal(
    y1,
    as.numeric(y2),
    tolerance = 1e-8
  )
  
})