
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

