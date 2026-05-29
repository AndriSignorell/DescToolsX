test_that("hoeffdingD returns a single numeric value", {
  set.seed(1)
  x <- rnorm(100); y <- rnorm(100)
  d <- hoeffdingD(x, y)
  expect_length(d, 1)
  expect_true(is.numeric(d))
})

test_that("hoeffdingD is near 0 for independent variables", {
  set.seed(2)
  x <- rnorm(500); y <- rnorm(500)
  d <- hoeffdingD(x, y)
  expect_lt(abs(d), 0.02)
})

test_that("hoeffdingD is large and positive for a strong monotone relationship", {
  x <- 1:200
  y <- x + rnorm(200, sd = 0.01)
  d <- hoeffdingD(x, y)
  expect_gt(d, 0.01)
})

test_that("hoeffdingD is positive for a nonlinear (quadratic) relationship", {
  set.seed(3)
  x <- runif(200, -3, 3)
  y <- x^2 + rnorm(200, sd = 0.1)
  d <- hoeffdingD(x, y)
  expect_gt(d, 0)
})

test_that("hoeffdingD warns when ties are present and jitter = FALSE", {
  x <- c(1, 1, 2, 3, 4, 5, 5, 6, 7, 8)
  y <- 1:10
  expect_warning(hoeffdingD(x, y), "Ties")
})

test_that("hoeffdingD jitter = TRUE resolves ties without warning", {
  x <- c(1, 1, 2, 3, 4, 5, 5, 6, 7, 8)
  y <- 1:10
  expect_no_warning(hoeffdingD(x, y, jitter = TRUE))
})

test_that("hoeffdingD jitter seed produces reproducible results", {
  set.seed(10)
  x <- round(rnorm(100))
  y <- round(rnorm(100))
  d1 <- hoeffdingD(x, y, jitter = TRUE, seed = 42)
  d2 <- hoeffdingD(x, y, jitter = TRUE, seed = 42)
  expect_equal(d1, d2)
})

test_that("hoeffdingD stops when x and y have different lengths", {
  expect_error(hoeffdingD(1:10, 1:5), "same length")
})

test_that("hoeffdingD stops for fewer than 5 observations", {
  expect_error(hoeffdingD(1:4, 1:4), "5")
})
