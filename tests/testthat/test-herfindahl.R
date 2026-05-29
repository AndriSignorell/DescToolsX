test_that("herfindahl returns 1 for monopoly (single firm)", {
  expect_equal(herfindahl(100), 1)
})

test_that("herfindahl returns 1/n for equal shares (minimum concentration)", {
  n <- 5
  x <- rep(20, n)
  expect_equal(herfindahl(x), 1/n, tolerance = 1e-10)
})

test_that("herfindahl result is in [1/n, 1]", {
  x <- c(10, 20, 30, 40)
  h <- herfindahl(x)
  expect_gte(h, 1/length(x))
  expect_lte(h, 1)
})

test_that("herfindahl increases with higher concentration", {
  x_equal  <- rep(25, 4)
  x_skewed <- c(85, 5, 5, 5)
  expect_lt(herfindahl(x_equal), herfindahl(x_skewed))
})

test_that("herfindahl returns NA for negative values", {
  expect_true(is.na(herfindahl(c(10, -5, 20))))
})

test_that("herfindahl returns NA when NAs present and na.rm = FALSE", {
  expect_true(is.na(herfindahl(c(10, NA, 20))))
})

test_that("herfindahl na.rm = TRUE strips NAs", {
  x <- c(10, NA, 20, 30)
  expect_equal(herfindahl(x, na.rm = TRUE), herfindahl(c(10, 20, 30)))
})

test_that("herfindahl frequency weights n replicate observations", {
  x <- c(10, 20)
  expect_equal(herfindahl(x, n = c(2, 2)), herfindahl(rep(x, c(2,2))))
})

test_that("herfindahl parameter argument changes the result", {
  x <- c(10, 20, 30, 40)
  h1 <- herfindahl(x, parameter = 1)
  h2 <- herfindahl(x, parameter = 2)
  expect_false(isTRUE(all.equal(h1, h2)))
})
