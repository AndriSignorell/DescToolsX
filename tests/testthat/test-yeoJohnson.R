
library(testthat)

## --- basic functionality ---

test_that("yeoJohnson returns numeric vector of same length", {
  x <- rnorm(100)
  y <- yeoJohnson(x, lambda = 0.5)
  
  expect_type(y, "double")
  expect_equal(length(y), length(x))
})

## --- inverse correctness ---

test_that("yeoJohnson and inverse recover original values", {
  set.seed(1)
  x <- rnorm(100)
  
  for (lambda in c(-1, 0, 0.5, 1, 2)) {
    y <- yeoJohnson(x, lambda)
    x2 <- yeoJohnsonInv(y, lambda)
    
    expect_equal(x, x2, tolerance = 1e-8)
  }
})

## --- lambda approx zero (log-like case for x >= 0) ---

test_that("lambda near zero behaves like log(x+1) for non-negative values", {
  x <- runif(100, 0, 10)
  
  y1 <- yeoJohnson(x, lambda = 0)
  y2 <- yeoJohnson(x, lambda = 1e-10)
  
  expect_equal(y1, log(x + 1))
  expect_equal(y2, log(x + 1), tolerance = 1e-8)
})

## --- lambda approx two (log-like case for x < 0) ---

test_that("lambda near two behaves like -log(-x+1) for negative values", {
  x <- -runif(100, 0, 10)
  
  y1 <- yeoJohnson(x, lambda = 2)
  y2 <- yeoJohnson(x, lambda = 2 - 1e-10)
  
  expect_equal(y1, -log(-x + 1))
  expect_equal(y2, -log(-x + 1), tolerance = 1e-8)
})

## --- NA handling ---

test_that("NA values are preserved", {
  x <- c(-1, 0, NA, 2)
  
  y <- yeoJohnson(x, lambda = 0.5)
  
  expect_true(is.na(y[3]))
})

## --- all NA error ---

test_that("all NA input throws error", {
  x <- c(NA, NA)
  
  expect_error(
    yeoJohnson(x, lambda = 0.5),
    "must be numeric|NA"
  )
})

## --- empty input ---

test_that("empty input returns empty output", {
  x <- numeric(0)
  
  y <- yeoJohnson(x, lambda = 0.5)
  
  expect_equal(y, numeric(0))
})

## --- invalid lambda ---

test_that("invalid lambda throws error", {
  x <- rnorm(10)
  
  expect_error(yeoJohnson(x, lambda = c(1,2)))
  expect_error(yeoJohnson(x, lambda = NA))
  expect_error(yeoJohnson(x, lambda = Inf))
})

## --- inverse NA handling ---

test_that("inverse preserves NA", {
  x <- c(1, NA, -2)
  
  y <- yeoJohnsonInv(x, lambda = 0.5)
  
  expect_true(is.na(y[2]))
})

## --- inverse all NA ---

test_that("inverse errors on all NA", {
  x <- c(NA, NA)
  
  expect_error(
    yeoJohnsonInv(x, lambda = 0.5),
    "must be numeric|NA"
  )
})

## --- monotonicity (sanity check) ---

test_that("yeoJohnson is monotone increasing", {
  x <- seq(-5, 5, length.out = 100)
  y <- yeoJohnson(x, lambda = 0.5)
  
  expect_true(all(diff(y) >= -1e-10))
})