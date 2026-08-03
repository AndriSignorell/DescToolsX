test_that("impute default (median) replaces NAs", {
  x <- c(2, 3, NA, 5, 9)
  res <- impute(x)
  expect_false(any(is.na(res)))
})

test_that("impute with median gives correct replacement value", {
  x <- c(2, 3, NA, 5, 9)
  expect_equal(impute(x)[3], median(c(2,3,5,9)))
})

test_that("impute preserves non-NA values", {
  x <- c(2, 3, NA, 5)
  res <- impute(x)
  expect_equal(res[c(1,2,4)], c(2, 3, 5))
})

test_that("impute FUN = mean replaces with mean", {
  x <- c(0, 10, NA)
  expect_equal(impute(x, FUN = mean)[3], 5)
})

test_that("impute FUN = scalar value replaces with that value", {
  x <- c(1, NA, 3)
  expect_equal(impute(x, FUN = 99)[2], 99)
})

test_that("impute returns same length as input", {
  x <- c(1, NA, NA, 4, 5)
  expect_length(impute(x), length(x))
})

test_that("impute stops when FUN returns non-scalar", {
  x <- c(1, 2, NA)
  expect_error(impute(x, FUN = function(x, ...) c(1,2)))
})

test_that("impute stops when FUN is not a function or scalar", {
  expect_error(impute(c(1, NA), FUN = c(1, 2)))
})

test_that("impute returns unchanged vector when no NAs present", {
  x <- c(1, 2, 3)
  expect_equal(impute(x), x)
})



test_that("impute decides on na.rm from the formals, not from an error", {
  
  x <- c(2, 3, NA, 5, 9)
  
  # impute() returns the VECTOR with the holes filled, not the value it
  # filled them with - I had asserted the latter
  expect_equal(impute(x), replace(x, is.na(x), median(x, na.rm = TRUE)))
  expect_equal(impute(x, mean), replace(x, is.na(x), mean(x, na.rm = TRUE)))
  expect_equal(impute(x, 99), c(2, 3, 99, 5, 9))
  
  # A function that has no na.rm is called without it ...
  noNaRm <- function(z) 42
  expect_equal(impute(x, noNaRm), c(2, 3, 42, 5, 9))
  
  # ... but a function that fails for its OWN reasons must surface that
  # error, not be silently retried without na.rm. The former tryCatch
  # swallowed everything.
  boom <- function(z, na.rm = TRUE) stop("deliberate failure")
  expect_error(impute(x, boom), "deliberate failure")
})

