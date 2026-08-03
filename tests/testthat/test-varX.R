test_that("varX() agrees with stats::var() for the unweighted case", {

  expect_equal(varX(1:10), var(1:10))
  expect_equal(varX(1:10), 9.166667, tolerance = 1e-6)
  expect_equal(sdX(1:10), sd(1:10))

  x <- c(2.3, 5.1, -0.7, 4.4, 9.9)
  expect_equal(varX(x), var(x))

})


test_that("estimator = 'ml' works without weights", {

  # regression: 'ok' only existed when na.rm was TRUE, so
  # varX(x, estimator = "ml") failed with "object 'ok' not found"
  expect_equal(varX(1:10, estimator = "ml"), var(1:10) * 9/10)
  expect_equal(sdX(1:10, estimator = "ml"), sqrt(var(1:10) * 9/10))

  x <- c(1, 2, NA, 4, 5)
  expect_equal(varX(x, estimator = "ml", na.rm = TRUE),
               var(c(1, 2, 4, 5)) * 3/4)
  expect_true(is.na(varX(x, estimator = "ml")))

})


test_that("weights are frequency weights", {

  expect_equal(varX(1:5, weights = 1:5), var(rep(1:5, times = 1:5)))
  expect_equal(varX(1:5, weights = 1:5), 1.666667, tolerance = 1e-6)
  expect_equal(sdX(1:5, weights = 1:5), sd(rep(1:5, times = 1:5)))

  set.seed(45)
  x <- sample(-10:20, size = 50, replace = TRUE)
  w <- table(x)
  z <- as.numeric(names(w))
  expect_equal(varX(z, weights = as.numeric(w)), varX(x))

  # ml with weights
  expect_equal(varX(1:5, weights = 1:5, estimator = "ml"),
               varX(rep(1:5, times = 1:5), estimator = "ml"))

})


test_that("na.rm keeps the shape of matrix input", {

  m <- cbind(a = c(1, 2, 3, NA), b = c(4, 5, 6, 7))

  expect_equal(dim(varX(m, na.rm = TRUE)), c(2L, 2L))
  expect_equal(varX(m, na.rm = TRUE), var(m, na.rm = TRUE))
  expect_equal(dim(varX(m)), c(2L, 2L))

  # ml on complete cases
  expect_equal(varX(m, na.rm = TRUE, estimator = "ml"),
               var(m, na.rm = TRUE) * 2/3)

})


test_that("varX() validates weights", {

  expect_error(varX(1:5, weights = 1:4), "same length")
  expect_error(varX(1:5, weights = c(1, 1, 1, 1, -1)), "non-negative")
  expect_error(varX(1:5, weights = c(1, 1, 1, 1, NA)), "non-negative")
  expect_error(varX(cbind(1:5, 2:6), weights = 1:5), "not supported")

})


test_that("degenerate input gives NA", {

  expect_true(is.na(varX(1)))
  expect_true(is.na(varX(3, weights = 1)))

})


test_that("varX.Freq checks its breaks", {

  fr <- freq(as.table(c(6, 16, 24, 25, 17)))

  expect_error(varX(fr), "required")
  expect_error(varX(fr, breaks = c(0, 10, 20)), "length nrow")

  brk <- c(0, 10, 20, 30, 40, 50)
  mid <- head(moveAvg(brk, order = 2, align = "left"), -1)
  n   <- sum(fr$freq)
  mu  <- sum(mid * fr$perc)

  expect_equal(varX(fr, breaks = brk),
               (sum(mid^2 * fr$freq) - n * mu^2) / (n - 1))
  expect_equal(sdX(fr, breaks = brk), sqrt(varX(fr, breaks = brk)))

})
