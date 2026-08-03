
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

test_that("iqrX with uniform integer weights equals unweighted result", {
  # quantileX() expects frequency-style weights (not normalized proportions)
  # diff() on a named quantile vector retains the name "75%" → strip with unname()
  x <- c(1, 3, 5, 7, 9)
  w <- rep(1, 5)
  expect_equal(unname(iqrX(x, weights = w)), iqrX(x), tolerance = 1e-6)
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

