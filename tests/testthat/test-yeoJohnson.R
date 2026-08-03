
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

test_that("lambda = 1 is the identity", {
  
  x <- c(-3.5, -1, 0, 1, 2.25, 10)
  expect_equal(yeoJohnson(x, lambda = 1), x)
  expect_equal(yeoJohnsonInv(x, lambda = 1), x)
  
})


test_that("the branches follow the published definition", {
  
  x <- c(-3.5, -1, 0, 1, 2.25, 10)
  lam <- 0.5
  
  ref <- ifelse(x >= 0,
                ((x + 1)^lam - 1)/lam,
                -(((-x + 1)^(2 - lam) - 1)/(2 - lam)))
  expect_equal(yeoJohnson(x, lambda = lam), ref)
  
  # lambda = 0 / lambda = 2 special cases
  expect_equal(yeoJohnson(c(0, 1, 3), lambda = 0), log(c(0, 1, 3) + 1))
  expect_equal(yeoJohnson(c(-1, -4), lambda = 2), -log(c(2, 5)))
  
  # the special cases are the limits of the general ones
  expect_equal(yeoJohnson(2, lambda = 0), yeoJohnson(2, lambda = 1e-7, tol = 0),
               tolerance = 1e-6)
  expect_equal(yeoJohnson(-2, lambda = 2), yeoJohnson(-2, lambda = 2 - 1e-7, tol = 0),
               tolerance = 1e-6)
  
})


test_that("the transformation is monotone and maps 0 to 0", {
  
  for(lam in c(-1, 0, 0.5, 1, 2, 3)){
    x <- seq(-5, 5, by = 0.25)
    y <- yeoJohnson(x, lambda = lam)
    expect_true(all(diff(y) > 0))
    expect_equal(yeoJohnson(0, lambda = lam), 0)
  }
  
})


test_that("yeoJohnsonInv() inverts yeoJohnson()", {
  
  set.seed(1)
  x <- rnorm(200, sd = 3)
  
  for(lam in c(-1, 0, 0.3, 1, 1.7, 2, 3))
    expect_equal(yeoJohnsonInv(yeoJohnson(x, lambda = lam), lambda = lam), x)
  
})


test_that("names and dimensions are preserved", {
  
  x <- c(a = -1, b = 0, c = 2)
  expect_equal(names(yeoJohnson(x, lambda = 0.5)), c("a", "b", "c"))
  expect_equal(names(yeoJohnsonInv(yeoJohnson(x, 0.5), 0.5)), c("a", "b", "c"))
  
  m <- matrix(c(-2, -1, 0, 1), nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  y <- yeoJohnson(m, lambda = 0.5)
  expect_equal(dim(y), c(2L, 2L))
  expect_equal(dimnames(y), dimnames(m))
  
})


test_that("NA values propagate", {
  
  x <- c(1, NA, -1)
  y <- yeoJohnson(x, lambda = 0.5)
  expect_true(is.na(y[2]))
  expect_equal(y[-2], yeoJohnson(c(1, -1), lambda = 0.5))
  
})


test_that("out of range values cannot be inverted", {
  
  # for lambda < 0 the image is bounded above by -1/lambda
  expect_error(yeoJohnsonInv(5, lambda = -0.5), "preimage")
  expect_silent(yeoJohnsonInv(1.5, lambda = -0.5))
  
})


test_that("yeoJohnson() validates its arguments", {
  
  expect_error(yeoJohnson(letters[1:3], lambda = 1), "numeric")
  expect_error(yeoJohnson(1:3, lambda = c(1, 2)), "single finite")
  expect_error(yeoJohnson(1:3, lambda = NA), "single finite")
  expect_error(yeoJohnson(1:3, lambda = 1, tol = -1), "non-negative")
  expect_error(yeoJohnson(c(NA, NA), lambda = 1), "only NA")
  expect_equal(yeoJohnson(numeric(0), lambda = 1), numeric(0))
  
})

