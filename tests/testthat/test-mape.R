test_that("mape returns 0 for perfect predictions", {
  x <- c(3, 5, 7)
  expect_equal(mape(x, ref = x), 0)
})

test_that("mape returns the correct value for a known case", {
  # each prediction off by 1/ref fraction
  x   <- c(2, 4)
  ref <- c(4, 4)   # errors: |4-2|/4=0.5, |4-4|/4=0
  expect_equal(mape(x, ref = ref), 0.25)
})

test_that("mape is non-negative", {
  x   <- c(1, 5, 3)
  ref <- c(3, 4, 2)
  expect_gte(mape(x, ref = ref), 0)
})

test_that("mape ref = 0 results in NA for that element", {
  x   <- c(1, 2)
  ref <- c(0, 4)
  res <- mape(x, ref = ref, na.rm = TRUE)
  expect_true(is.numeric(res))
})

test_that("mape na.rm = TRUE handles NAs", {
  x   <- c(1, NA, 3)
  ref <- c(2, 4,  4)
  expect_equal(mape(x, ref = ref, na.rm = TRUE),
               mape(c(1,3), c(2,4)))
})

test_that("mape stops when x and ref have different lengths", {
  expect_error(mape(1:3, ref = 1:4), "same length")
})

test_that("mape.lm returns a non-negative numeric for a linear model", {
  fit <- lm(mpg ~ hp, data = mtcars)
  res <- mape(fit)
  expect_gte(res, 0)
  expect_length(res, 1)
})



test_that("mape is a fraction and mae is on the data scale", {
  
  x   <- c(2.5, 3.0, 2.8)
  ref <- c(3.0, 2.5, 3.0)
  
  expect_equal(mae(x, ref), mean(abs(ref - x)))
  expect_equal(mape(x, ref), mean(abs((ref - x) / ref)))
  expect_lt(mape(x, ref), 1)          # a fraction, not a percentage
  
  # a zero reference is NA, and a missing one does not break the index
  expect_true(is.na(mape(c(1, 2), c(0, 2))))
  expect_equal(mape(c(1, 2, 3), c(0, 2, NA), na.rm = TRUE),
               mean(abs((2 - 2) / 2)))
})
