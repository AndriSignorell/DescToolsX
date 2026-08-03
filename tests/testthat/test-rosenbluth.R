
test_that("rosenbluth returns a single positive numeric", {
  x <- c(10, 20, 30, 40)
  res <- rosenbluth(x)
  expect_length(res, 1); expect_true(is.numeric(res))
  expect_gt(res, 0)
})

test_that("rosenbluth = 1/n for equal shares (minimum concentration)", {
  n <- 5
  x <- rep(20, n)
  expect_equal(rosenbluth(x), 1/n, tolerance = 1e-8)
})

test_that("rosenbluth is larger (more concentrated) for skewed distribution", {
  # Rosenbluth ranges from 1/n (equal shares) to 1 (monopoly) —
  # larger values indicate STRONGER concentration
  x_equal  <- rep(10, 5)
  x_skewed <- c(80, 5, 5, 5, 5)
  expect_gt(rosenbluth(x_skewed), rosenbluth(x_equal))
})

test_that("rosenbluth returns NA for negative values", {
  expect_true(is.na(rosenbluth(c(10, -5, 20))))
})

test_that("rosenbluth returns NA when NAs present and na.rm = FALSE", {
  expect_true(is.na(rosenbluth(c(10, NA, 20))))
})

test_that("rosenbluth na.rm = TRUE strips NAs", {
  x <- c(10, NA, 20, 30)
  expect_equal(rosenbluth(x, na.rm = TRUE), rosenbluth(c(10, 20, 30)))
})

test_that("rosenbluth frequency weights replicate observations", {
  x <- c(10, 20)
  expect_equal(rosenbluth(x, n = c(2,2)), rosenbluth(rep(x, c(2,2))))
})


test_that("the index runs from 1/k for equal units to 1 for a monopoly", {
  
  expect_equal(rosenbluth(rep(1, 4)), 0.25)
  expect_equal(rosenbluth(rep(7, 10)), 0.1)
  expect_equal(rosenbluth(c(1, 0, 0, 0)), 1)
  expect_equal(rosenbluth(5), 1)
})


test_that("the index is scale invariant", {
  
  x <- c(10, 4, 3, 1)
  expect_equal(rosenbluth(x), rosenbluth(x * 1000))
})


test_that("the value matches the closed form", {
  
  x <- c(10, 1, 1, 1)
  s <- sort(x, decreasing = TRUE) / sum(x)
  expect_equal(rosenbluth(x),
               1 / (2 * sum(seq_along(s) * s) - 1))
  expect_equal(rosenbluth(x), 0.52)
})


test_that("frequency weights replicate the values", {
  
  expect_equal(rosenbluth(c(10, 1), n = c(1, 3)),
               rosenbluth(c(10, 1, 1, 1)))
})


test_that("degenerate input returns NA rather than NaN", {
  
  # All values zero has no shares to rank; this used to be a silent NaN.
  expect_true(is.na(rosenbluth(c(0, 0, 0))))
  expect_true(is.na(rosenbluth(numeric(0))))
  
  expect_true(is.na(rosenbluth(c(1, NA, 2))))
  expect_equal(rosenbluth(c(1, NA, 2), na.rm = TRUE), rosenbluth(c(1, 2)))
  
  expect_true(is.na(rosenbluth(c(1, -2, 3))))
})


test_that("arguments are validated", {
  
  expect_error(rosenbluth(letters), "numeric")
  expect_error(rosenbluth(c(1, 2), n = c(1, 2, 3)), "length")
  expect_error(rosenbluth(c(1, 2), n = c(1, -1)), "non-negative")
  expect_error(rosenbluth(c(1, 2), n = c(1.5, 2)), "whole numbers")
  expect_error(rosenbluth(c(1, 2), na.rm = NA), "na.rm")
})


test_that("the result is an unnamed scalar", {
  
  res <- rosenbluth(c(a = 3, b = 1))
  expect_null(names(res))
  expect_length(res, 1L)
})
