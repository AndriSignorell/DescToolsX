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
