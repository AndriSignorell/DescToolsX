
test_that("iqrX without weights equals base IQR()", {
  x <- c(1, 3, 5, 7, 9, 11, 13)
  expect_equal(iqrX(x), IQR(x))
})

test_that("iqrX is non-negative", {
  x <- rnorm(50)
  expect_gte(iqrX(x), 0)
})

test_that("iqrX is 0 for a constant vector", {
  expect_equal(iqrX(rep(5, 20)), 0)
})

test_that("iqrX na.rm = TRUE strips NAs", {
  x <- c(1, 3, NA, 7, 9)
  expect_equal(iqrX(x, na.rm = TRUE), IQR(x, na.rm = TRUE))
})

test_that("iqrX with equal weights equals the unweighted result of its type", {
  x <- c(1, 3, 5, 7, 9, 12)
  # weighted default is type 2, so compare against type 2 ...
  expect_equal(iqrX(x, weights = rep(1, 6)), iqrX(x, type = 2))
  expect_equal(iqrX(x, weights = rep(0.1, 6)), iqrX(x, type = 2))
  # ... and replication counts against type 7
  expect_equal(iqrX(x, weights = rep(1, 6), type = 7), IQR(x))
})


test_that("iqrX returns NA for missing values instead of stopping", {
  x <- c(1, 3, NA, 7, 9)
  expect_identical(iqrX(x), NA_real_)
  expect_identical(iqrX(x, weights = rep(1, 5)), NA_real_)
  expect_equal(iqrX(x, weights = rep(1, 5), na.rm = TRUE),
               iqrX(c(1, 3, 7, 9), weights = rep(1, 4)))
})

test_that("iqrX with weights returns a positive numeric", {
  x <- c(3.7, 3.3, 3.5, 2.8)
  w <- c(5, 5, 4, 1) / 15
  res <- iqrX(x, weights = w)
  expect_gte(res, 0)
  expect_length(res, 1)
})



test_that("iqrX returns the same shape with and without weights", {
  
  x <- c(3.7, 3.3, 3.5, 2.8)
  w <- c(5, 5, 4, 1) / 15
  
  plain <- iqrX(x)
  wtd   <- iqrX(x, weights = w)
  
  expect_null(names(plain))
  expect_null(names(wtd))        # was labelled "75%"
  expect_length(wtd, 1L)
  expect_gte(wtd, 0)
  
  # the weighted branch now depends only on the RATIOS of the weights
  expect_equal(iqrX(x, weights = w), iqrX(x, weights = w * 15))
  expect_equal(iqrX(x, weights = w), iqrX(x, weights = w / 3))
})



test_that("iqrX: NaN weights are missing, invalid weights still an error", {
  x <- c(1, 3, 5, 7, 9)
  expect_identical(iqrX(x, weights = c(1, 1, NaN, 1, 1)), NA_real_)
  expect_equal(iqrX(x, weights = c(1, 1, NaN, 1, 1), na.rm = TRUE),
               iqrX(x[-3], weights = rep(1, 4)))
  expect_error(iqrX(c(x, NA), weights = c(1, 1, 1, 1, -1, 1)), "negative")
})
