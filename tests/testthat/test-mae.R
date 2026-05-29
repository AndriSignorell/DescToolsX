test_that("mae returns 0 for perfect predictions", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(mae(x, ref = x), 0)
})

test_that("mae returns the mean absolute error", {
  x   <- c(1, 2, 3, 4, 5)
  ref <- c(2, 3, 4, 5, 6)   # all off by 1
  expect_equal(mae(x, ref = ref), 1)
})

test_that("mae is symmetric: mae(x, ref) == mae(ref, x)", {
  x   <- c(1, 3, 5)
  ref <- c(2, 5, 6)
  expect_equal(mae(x, ref = ref), mae(ref, ref = x))
})

test_that("mae is always non-negative", {
  x   <- c(-3, 0, 5)
  ref <- c(2, -1, 3)
  expect_gte(mae(x, ref = ref), 0)
})

test_that("mae na.rm = TRUE handles missing values", {
  x   <- c(1, 2, NA, 4)
  ref <- c(2, 3,  4, 5)
  expect_equal(mae(x, ref = ref, na.rm = TRUE),
               mae(c(1,2,4), c(2,3,5)))
})

test_that("mae stops when x and ref have different lengths", {
  expect_error(mae(1:5, ref = 1:4), "same length")
})

test_that("mae.lm returns a non-negative numeric for a linear model", {
  fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  result <- mae(fit)
  expect_gte(result, 0)
  expect_length(result, 1)
})
