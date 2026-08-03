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



test_that("smape matches its definition and its bounds", {
  
  x <- c(2.5, 3.0, 2.8)
  ref <- c(3.0, 2.5, 3.0)
  
  expect_equal(smape(x, ref),
               mean(2 * abs(ref - x) / (abs(x) + abs(ref))))
  
  # 0 for a perfect fit, 2 when one of the two is zero
  expect_equal(smape(x, x), 0)
  expect_equal(smape(c(0, 0), c(3, 5)), 2)
})


test_that("a zero/zero pair is undefined, not zero", {
  
  x <- c(0, 2, 3)
  ref <- c(0, 2, 4)
  
  # with na.rm = FALSE the undefined term propagates
  expect_true(is.na(smape(x, ref)))
  
  # with na.rm = TRUE it is dropped, and the mean is over two terms
  expect_equal(smape(x, ref, na.rm = TRUE),
               mean(c(0, 2 * 1 / 7)))
})


test_that("missing values do not break the zero test", {
  
  # denom is NA there, so the logical index carries an NA; with a length-one
  # replacement those positions are skipped rather than erroring.
  expect_true(is.na(smape(c(1, NA), c(2, 3))))
  expect_equal(smape(c(1, NA), c(2, 3), na.rm = TRUE),
               2 * 1 / 3)
})


test_that("a mistyped argument is not swallowed by mean()", {
  
  # '...' is no longer forwarded to mean(), so trim= and friends no longer
  # change the result silently.
  expect_equal(smape(c(1, 2, 100), c(2, 3, 1), trim = 0.5),
               smape(c(1, 2, 100), c(2, 3, 1)))
})


test_that("smape and rmse validate their arguments", {
  
  expect_error(smape(1:3, 1:4), "same length")
  expect_error(smape(letters, letters), "numeric")
  expect_error(smape(1:3, 1:3, na.rm = NA), "na.rm")
  expect_error(rmse(1:3, 1:4), "same length")
})


test_that("the lm methods refuse a non-numeric response", {
  
  fit <- lm(mpg ~ hp, data = mtcars)
  expect_equal(rmse(fit),
               sqrt(mean(residuals(fit)^2)))
  
  gfit <- glm(I(mpg > 20) ~ hp, data = mtcars, family = binomial)
  # a factor or matrix response reached the arithmetic unchecked before
  expect_error(rmse(gfit), "numeric vector")
  expect_error(smape(gfit), "numeric vector")
})
