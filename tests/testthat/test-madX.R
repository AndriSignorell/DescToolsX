test_that("madX returns a non-negative numeric", {
  x <- c(1, 2, 3, 4, 5)
  expect_gte(madX(x), 0)
  expect_length(madX(x), 1)
})

test_that("madX returns 0 for a constant vector", {
  expect_equal(madX(rep(5, 10)), 0)
})

test_that("madX without weights matches base mad() for simple data", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  expect_equal(madX(x), mad(x), tolerance = 1e-8)
})

test_that("madX is robust to outliers", {
  x_clean   <- c(1,2,3,4,5,6,7,8,9,10)
  x_outlier <- c(x_clean, 1000)
  # mad should not change much; mean/sd would explode
  expect_lt(abs(madX(x_outlier) - madX(x_clean)), 2)
})

test_that("madX na.rm = TRUE strips NAs", {
  x <- c(1, 2, NA, 4, 5)
  expect_equal(madX(x, na.rm = TRUE), madX(c(1, 2, 4, 5)))
})

test_that("madX uniform weights give same result as unweighted", {
  x <- c(2, 4, 6, 8, 10)
  w <- rep(1, 5)
  expect_equal(madX(x, weights = w), madX(x), tolerance = 1e-8)
})

test_that("madX constant is applied correctly: result = 1.4826 * raw MAD", {
  x <- c(1, 2, 3, 4, 5)
  res_default <- madX(x, constant = 1.4826)
  res_no_scale <- madX(x, constant = 1)
  expect_equal(res_default, 1.4826 * res_no_scale, tolerance = 1e-8)
})

test_that("madX medianType = 'low' and 'high' give valid results", {
  set.seed(1)
  x <- rnorm(20)
  res_low  <- madX(x, medianType = "low")
  res_high <- madX(x, medianType = "high")
  expect_gte(res_low, 0)
  expect_gte(res_high, 0)
})
