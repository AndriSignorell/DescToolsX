

test_that("modeX returns the single mode", {
  x <- c(0:5, 5)
  expect_equal(modeX(x), 5, ignore_attr = TRUE)
})

test_that("modeX returns all modes when multiple exist", {
  x <- c(1,1,2,2,3)
  res <- modeX(x)
  expect_true(all(c(1,2) %in% res))
})

test_that("modeX returns NA when there are no repeats (no mode)", {
  x <- 1:5
  expect_true(is.na(modeX(x)))
})

test_that("modeX returns NA for a single value", {
  expect_true(is.na(modeX(42)))
})

test_that("modeX returns NA when NA is present and na.rm = FALSE", {
  x <- c(1, 1, 2, NA)
  expect_true(is.na(modeX(x)))
})

test_that("modeX na.rm = TRUE strips NAs before computing mode", {
  x <- c(1, 1, 2, NA)
  expect_equal(modeX(x, na.rm = TRUE), 1, ignore_attr = TRUE)
})

test_that("modeX has a 'freq' attribute giving the modal frequency", {
  x <- c(1,1,1,2,2,3)
  res <- modeX(x)
  expect_equal(attr(res, "freq"), 3L)
})

test_that("modeX works for character vectors", {
  x <- c("a","a","b","c")
  expect_equal(modeX(x, na.rm = TRUE), "a", ignore_attr = TRUE)
})

test_that("modeX stops for matrix input", {
  expect_error(modeX(matrix(1:4, 2,2)))
})


test_that("modeX reports no mode when every value is unique", {
  
  # the guard was `length(res) == 0L & attr(res, "freq") == 1L`, which
  # cannot fire: with all values distinct the C++ returns them all
  expect_true(is.na(modeX(0:5)))
  expect_true(is.na(attr(modeX(0:5), "freq")))
  
  expect_equal(as.vector(modeX(c(0:5, 5))), 5)
  expect_equal(attr(modeX(c(0:5, 5)), "freq"), 2L)
  
  # several modes are all returned, in order
  expect_equal(as.vector(modeX(c(0:5, 4, 5, 6))), c(4, 5))
})

