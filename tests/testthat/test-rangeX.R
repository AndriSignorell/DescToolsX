
test_that("rangeX returns the full range for untrimmed data", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(rangeX(x), 4, ignore_attr = TRUE)
})

test_that("rangeX returns 0 for a constant vector", {
  expect_equal(rangeX(rep(5, 10)), 0, ignore_attr = TRUE)
})

test_that("rangeX trim reduces the range", {
  x <- c(0:10, 50)
  expect_lt(rangeX(x, trim = 0.1), rangeX(x))
})

test_that("rangeX has a 'bounds' attribute with min and max", {
  x <- 1:10
  res <- rangeX(x)
  b <- attr(res, "bounds")
  expect_length(b, 2)
  expect_equal(b, c(1, 10))
})

test_that("rangeX robust = TRUE returns a smaller range than standard for outlier data", {
  set.seed(1)
  x <- c(rnorm(20), 100)
  expect_lt(rangeX(x, robust = TRUE), rangeX(x))
})

test_that("rangeX robust = TRUE returns a non-negative value", {
  set.seed(2)
  x <- c(rnorm(30), rnorm(3, 5, 20))
  expect_gte(rangeX(x, robust = TRUE), 0)
})

test_that("rangeX robust result has a 'bounds' attribute", {
  x <- c(rnorm(20), 100)
  res <- rangeX(x, robust = TRUE)
  expect_false(is.null(attr(res, "bounds")))
})

test_that("rangeX na.rm = TRUE works for both modes", {
  x <- c(1, 2, NA, 4, 5)
  expect_equal(rangeX(x, na.rm = TRUE), rangeX(c(1,2,4,5)))
  expect_true(is.numeric(rangeX(x, robust = TRUE, na.rm = TRUE)))
})

