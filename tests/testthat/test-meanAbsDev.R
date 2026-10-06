test_that("meanAbsDev returns a non-negative numeric", {
  x <- c(2, 4, 6, 8)
  res <- meanAbsDev(x)
  expect_gte(res, 0)
  expect_length(res, 1)
})

test_that("meanAbsDev is 0 for a constant vector", {
  expect_equal(meanAbsDev(rep(5, 10)), 0)
})

test_that("meanAbsDev default (mean center) matches manual calculation", {
  x <- c(1, 3, 5, 7)
  expected <- mean(abs(x - mean(x)))
  expect_equal(meanAbsDev(x), expected, tolerance = 1e-10)
})

test_that("meanAbsDev center = median uses the median", {
  x <- c(1, 2, 5, 100)
  expected <- mean(abs(x - median(x)))
  expect_equal(meanAbsDev(x, center = median), expected, tolerance = 1e-10)
})

test_that("meanAbsDev center = scalar uses that scalar", {
  x <- c(1, 2, 3, 4, 5)
  center <- 3
  expected <- mean(abs(x - center))
  expect_equal(meanAbsDev(x, center = center), expected, tolerance = 1e-10)
})

test_that("meanAbsDev na.rm = TRUE strips NAs", {
  x <- c(2, 4, NA, 8)
  expect_equal(meanAbsDev(x, na.rm = TRUE), meanAbsDev(c(2,4,8)))
})

test_that("meanAbsDev uniform weights give same result as unweighted", {
  x <- c(2, 4, 6, 8)
  expect_equal(meanAbsDev(x, weights = rep(1, 4)), meanAbsDev(x), tolerance = 1e-6)
})

test_that("meanAbsDev frequency weights match replicated unweighted", {
  x <- c(0:6)
  w <- c(21, 46, 54, 40, 24, 10, 5)
  expect_equal(meanAbsDev(x = x, weights = w),
               meanAbsDev(rep(x, w)), tolerance = 1e-6)
})


test_that("meanAbsDev keeps x and weights aligned under na.rm", {
  
  x <- c(2, 3, NA, 5, 9)
  w <- c(1, 1, 99, 1, 1)     # the big weight sits on the missing value
  
  # x was filtered and weights were not, so every observation was paired
  # with the wrong weight from here on - including inside the center
  expect_equal(meanAbsDev(x, weights = w, na.rm = TRUE),
               meanAbsDev(c(2, 3, 5, 9), weights = c(1, 1, 1, 1)))
  
  # unweighted behaviour is unchanged
  expect_equal(meanAbsDev(x, na.rm = TRUE), meanAbsDev(c(2, 3, 5, 9)))
  expect_equal(meanAbsDev(c(2, 3, 5, 9)), mean(abs(c(2, 3, 5, 9) - mean(c(2, 3, 5, 9)))))
})


test_that("meanAbsDev accepts a function or a fixed center", {
  x <- c(2, 3, 5, 3, 1, 15, 23)
  expect_equal(meanAbsDev(x, center = mean), mean(abs(x - mean(x))))
  expect_equal(meanAbsDev(x, center = median), mean(abs(x - median(x))))
  expect_equal(meanAbsDev(x, center = 4), mean(abs(x - 4)))
})
