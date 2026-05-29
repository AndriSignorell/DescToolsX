test_that("rmse returns 0 for perfect predictions", {
  x <- c(1, 2, 3)
  expect_equal(rmse(x, ref = x), 0)
})

test_that("rmse = sqrt(mse)", {
  x   <- c(1, 3, 5)
  ref <- c(2, 4, 6)
  expect_equal(rmse(x, ref), sqrt(mse(x, ref)))
})

test_that("rmse is always non-negative", {
  x   <- rnorm(50)
  ref <- rnorm(50)
  expect_gte(rmse(x, ref), 0)
})

test_that("rmse is symmetric: rmse(x, ref) == rmse(ref, x)", {
  x   <- c(1, 3, 5)
  ref <- c(2, 4, 6)
  expect_equal(rmse(x, ref), rmse(ref, x))
})

test_that("rmse is in same units as x (unlike mse)", {
  x   <- c(100, 200, 300)
  ref <- c(110, 190, 310)
  expect_lt(rmse(x, ref), 20)   # errors are ~10, RMSE should be ~10
})

test_that("rmse na.rm = TRUE handles NAs", {
  x   <- c(1, NA, 3)
  ref <- c(2, 5,  4)
  expect_equal(rmse(x, ref, na.rm = TRUE),
               rmse(c(1,3), c(2,4)))
})

test_that("rmse stops when x and ref have different lengths", {
  expect_error(rmse(1:3, ref = 1:4), "same length")
})

test_that("rmse.lm returns a non-negative numeric", {
  fit <- lm(mpg ~ hp, data = mtcars)
  res <- rmse(fit)
  expect_gte(res, 0); expect_length(res, 1)
})
