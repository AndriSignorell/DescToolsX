test_that("theil returns 0 for perfect equality", {
  expect_equal(theil(rep(10, 20)), 0, tolerance = 1e-10)
})

test_that("theil is non-negative for positive data", {
  x <- c(1, 5, 10, 50, 100)
  expect_gte(theil(x), 0)
})

test_that("theil increases with greater inequality", {
  x_equal  <- rep(10, 5)
  x_skewed <- c(1, 1, 1, 1, 100)
  expect_lt(theil(x_equal), theil(x_skewed))
})

test_that("theil returns NA for negative values", {
  expect_true(is.na(theil(c(1, -2, 3))))
})

test_that("theil returns 0 when mean is 0 (all zeros)", {
  expect_equal(theil(rep(0, 5)), 0)
})

test_that("theil returns NA when NAs present and na.rm = FALSE", {
  expect_true(is.na(theil(c(1, NA, 3))))
})

test_that("theil na.rm = TRUE strips NAs", {
  x <- c(1, NA, 3, 4)
  expect_equal(theil(x, na.rm = TRUE), theil(c(1, 3, 4)))
})

test_that("theil frequency weights replicate observations", {
  x <- c(10, 20)
  expect_equal(theil(x, n = c(3,3)), theil(rep(x, c(3,3))), tolerance=1e-10)
})


test_that("theil() matches the definition for strictly positive data", {
  
  x <- c(1, 2, 3, 4, 5)
  r <- x / mean(x)
  expect_equal(theil(x), mean(r * log(r)))
  
  y <- c(12, 7, 33, 5, 90, 21)
  ry <- y / mean(y)
  expect_equal(theil(y), mean(ry * log(ry)))
  
})


test_that("theil() is 0 under perfect equality and log(n) at the maximum", {
  
  expect_equal(theil(rep(10, 4)), 0)
  expect_equal(theil(rep(1, 17)), 0)
  
  # everything in one hand -> log(n)
  expect_equal(theil(c(0, 0, 0, 40)), log(4))
  expect_equal(theil(c(rep(0, 9), 1)), log(10))
  
})


test_that("zero values follow the convention 0*log(0) = 0", {
  
  # regression: r*log(r) was NaN for a zero value, so a single zero
  # turned the whole index into NaN
  expect_false(is.nan(theil(c(0, 1, 2, 3))))
  expect_true(is.finite(theil(c(0, 1, 2, 3))))
  
  x <- c(0, 1, 2, 3)
  r <- x / mean(x)
  expect_equal(theil(x), mean(ifelse(r == 0, 0, r * log(r))))
  
  # all zero: no inequality
  expect_equal(theil(rep(0, 5)), 0)
  
})


test_that("frequency weights replicate the observations", {
  
  expect_equal(theil(1:3, n = c(1, 2, 3)),
               theil(rep(1:3, times = c(1, 2, 3))))
  
  expect_equal(theil(c(5, 10), n = c(3, 1)),
               theil(c(5, 5, 5, 10)))
  
  # zero weights drop an observation
  expect_equal(theil(c(1, 2, 3), n = c(1, 0, 1)), theil(c(1, 3)))
  
})


test_that("theil() validates its arguments", {
  
  expect_error(theil(letters[1:3]), "numeric")
  expect_error(theil(1:3, n = 1:2), "same length")
  expect_error(theil(1:3, n = c(1, 1.5, 1)), "integer")
  expect_error(theil(1:3, n = c(1, -1, 1)), "integer")
  expect_error(theil(1:3, n = c(1, NA, 1)), "integer")
  
})


test_that("missing and negative values return NA", {
  
  expect_true(is.na(theil(c(1, 2, NA))))
  expect_true(is.na(theil(c(1, -2, 3))))
  expect_true(is.na(theil(numeric(0))))
  expect_true(is.na(theil(NA_real_, na.rm = TRUE)))
  
  expect_equal(theil(c(1, 2, 3, NA), na.rm = TRUE), theil(1:3))
  
})

