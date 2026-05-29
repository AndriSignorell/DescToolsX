test_that("meanAD returns a non-negative numeric", {
  x <- c(2, 4, 6, 8)
  res <- meanAD(x)
  expect_gte(res, 0)
  expect_length(res, 1)
})

test_that("meanAD is 0 for a constant vector", {
  expect_equal(meanAD(rep(5, 10)), 0)
})

test_that("meanAD default (mean center) matches manual calculation", {
  x <- c(1, 3, 5, 7)
  expected <- mean(abs(x - mean(x)))
  expect_equal(meanAD(x), expected, tolerance = 1e-10)
})

test_that("meanAD center = median uses the median", {
  x <- c(1, 2, 5, 100)
  expected <- mean(abs(x - median(x)))
  expect_equal(meanAD(x, center = median), expected, tolerance = 1e-10)
})

test_that("meanAD center = scalar uses that scalar", {
  x <- c(1, 2, 3, 4, 5)
  center <- 3
  expected <- mean(abs(x - center))
  expect_equal(meanAD(x, center = center), expected, tolerance = 1e-10)
})

test_that("meanAD na.rm = TRUE strips NAs", {
  x <- c(2, 4, NA, 8)
  expect_equal(meanAD(x, na.rm = TRUE), meanAD(c(2,4,8)))
})

test_that("meanAD uniform weights give same result as unweighted", {
  x <- c(2, 4, 6, 8)
  expect_equal(meanAD(x, weights = rep(1, 4)), meanAD(x), tolerance = 1e-6)
})

test_that("meanAD frequency weights match replicated unweighted", {
  x <- c(0:6)
  w <- c(21, 46, 54, 40, 24, 10, 5)
  expect_equal(meanAD(x = x, weights = w),
               meanAD(rep(x, w)), tolerance = 1e-6)
})
