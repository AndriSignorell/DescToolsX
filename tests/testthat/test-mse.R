test_that("mse returns 0 for perfect predictions", {
  x <- c(1, 2, 3)
  expect_equal(mse(x, ref = x), 0)
})

test_that("mse returns the correct value for a known case", {
  x   <- c(1, 2, 3)
  ref <- c(4, 5, 6)   # all errors = 3, MSE = 9
  expect_equal(mse(x, ref = ref), 9)
})

test_that("mse is non-negative", {
  x   <- rnorm(50)
  ref <- rnorm(50)
  expect_gte(mse(x, ref = ref), 0)
})

test_that("mse is symmetric: mse(x, ref) == mse(ref, x)", {
  x   <- c(1, 3, 5)
  ref <- c(2, 4, 6)
  expect_equal(mse(x, ref = ref), mse(ref, ref = x))
})

test_that("mse na.rm = TRUE handles NAs", {
  x   <- c(1, NA, 3)
  ref <- c(2, 5,  4)
  expect_equal(mse(x, ref = ref, na.rm = TRUE),
               mse(c(1,3), c(2,4)))
})

test_that("mse stops when x and ref have different lengths", {
  expect_error(mse(1:3, ref = 1:4), "same length")
})

test_that("mse.lm returns a non-negative numeric", {
  fit <- lm(mpg ~ hp, data = mtcars)
  res <- mse(fit)
  expect_gte(res, 0); expect_length(res, 1)
})
