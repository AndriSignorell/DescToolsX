test_that("logSt returns a numeric vector of same length as x", {
  x <- c(0.001, 0.01, 0.1, 1, 10, 100)
  res <- logSt(x)
  expect_type(res, "double")
  expect_length(res, length(x))
})

test_that("logSt above threshold equals log(x)", {
  x     <- c(1, 10, 100, 1000)
  res   <- logSt(x, threshold = 0.5)
  above <- x > 0.5
  expect_equal(res[above], log10(x[above]), tolerance = 1e-10)
})

test_that("logSt is continuous at the threshold", {
  x <- c(0.001, 0.01, 0.1, 0.5, 1, 10)
  thr <- 0.1
  res  <- logSt(x, threshold = thr)
  # value just below and just at threshold should be close
  expect_equal(res[x == thr], log10(thr), tolerance = 0.01)
})

test_that("logSt threshold attribute is set on the result", {
  x   <- c(0.01, 0.1, 1, 10)
  res <- logSt(x)
  expect_false(is.null(attr(res, "threshold")))
})

test_that("logSt base attribute is set on the result", {
  x   <- c(0.01, 1, 100)
  res <- logSt(x, base = 10)
  expect_equal(attr(res, "base"), 10)
})

test_that("logSt base = exp(1) gives natural-log-equivalent above threshold", {
  x   <- c(1, 10, 100)
  res <- logSt(x, base = exp(1), threshold = 0.5)
  expect_equal(res[x > 0.5], log(x[x > 0.5]), tolerance = 1e-10)
})

test_that("logSt handles NAs by returning NA for those positions", {
  x   <- c(1, NA, 100)
  res <- logSt(x, threshold = 0.5)
  expect_true(is.na(res[2]))
})

test_that("logSt custom threshold is respected", {
  x   <- c(0.001, 0.1, 1, 10)
  thr <- 1.0
  res <- logSt(x, threshold = thr)
  expect_equal(attr(res, "threshold"), thr)
})
