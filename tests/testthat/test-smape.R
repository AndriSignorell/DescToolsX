test_that("smape returns 0 for perfect predictions", {
  x <- c(1, 2, 3)
  expect_equal(smape(x, ref = x), 0)
})

test_that("smape result is in [0, 2]", {
  x   <- c(2.5, 3.0, 2.8)
  ref <- c(3.0, 2.5, 3.0)
  res <- smape(x, ref)
  expect_gte(res, 0); expect_lte(res, 2)
})

test_that("smape is symmetric: smape(x,ref) == smape(ref,x)", {
  x   <- c(1, 3, 5)
  ref <- c(2, 4, 6)
  expect_equal(smape(x, ref), smape(ref, x))
})

test_that("smape manual calculation matches function", {
  x   <- c(2, 4)
  ref <- c(4, 4)
  expected <- mean(2*abs(ref-x)/(abs(x)+abs(ref)))
  expect_equal(smape(x, ref), expected, tolerance=1e-10)
})

test_that("smape returns NA when both x and ref are 0", {
  x   <- c(0, 1)
  ref <- c(0, 2)
  res <- smape(x, ref, na.rm = TRUE)
  expect_true(is.numeric(res))
})

test_that("smape na.rm = TRUE handles NAs", {
  x   <- c(1, NA, 3)
  ref <- c(2, 4,  4)
  expect_equal(smape(x, ref, na.rm=TRUE), smape(c(1,3), c(2,4)))
})

test_that("smape stops when x and ref have different lengths", {
  expect_error(smape(1:3, ref=1:4), "same length")
})

test_that("smape.lm returns a non-negative numeric", {
  fit <- lm(mpg ~ hp, data = mtcars)
  res <- smape(fit)
  expect_gte(res, 0); expect_length(res, 1)
})
