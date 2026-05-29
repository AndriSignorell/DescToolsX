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
