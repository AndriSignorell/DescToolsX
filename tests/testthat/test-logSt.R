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



test_that("logSt refuses data it cannot derive a threshold from", {
  
  expect_error(logSt(c(0, -1, -2)), "no positive values")
  
  x <- c(0, 0.001, 1, 10, 100)
  y <- logSt(x)
  expect_false(anyNA(y))
  expect_type(y, "double")
  
  # round trip
  expect_equal(logStInv(y), x, tolerance = 1e-8)
})


test_that("logStInv insists on threshold and base when they were dropped", {
  
  y <- logSt(c(0, 0.001, 1, 10, 100))
  
  # NOT any arithmetic: `y + 0` KEEPS the attributes, which my first
  # version of this test asserted otherwise. Subsetting and as.vector()
  # do drop them, and those are just as ordinary a thing to do to a
  # transformed vector.
  expect_false(is.null(attr(y + 0, "threshold")))
  
  bare <- as.vector(y)
  expect_null(attr(bare, "threshold"))
  expect_null(attr(y[1:3], "threshold"))
  expect_error(logStInv(bare), "does not carry")
  
  expect_silent(logStInv(bare, base = attr(y, "base"),
                         threshold = attr(y, "threshold")))
})

