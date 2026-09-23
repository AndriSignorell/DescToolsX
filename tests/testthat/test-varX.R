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

  # scale matters: three weights of 4/3 are four observations
  expect_equal(varX(1:3, weights = rep(4/3, 3)), 8/9)

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
  expect_error(varX(cbind(1:5, 2:6), weights = 1:5), "not supported")

})


test_that("degenerate input gives NA", {

  expect_true(is.na(varX(1)))
  expect_true(is.na(varX(3, weights = 1)))

  # total weight <= 1 is at most one observation, for both estimators
  expect_identical(varX(1:3, weights = rep(1/3, 3)), NA_real_)
  expect_identical(varX(1:3, weights = rep(1/3, 3), estimator = "ml"), NA_real_)
  expect_identical(varX(5, weights = 1, estimator = "ml"), NA_real_)

  # nothing left after na.rm
  expect_identical(varX(c(NA, NA), weights = c(1, 1), na.rm = TRUE), NA_real_)

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


# Cases from the DescTools SD() issue (NA handling and scale of weights)

v  <- c(1, NA, 2, NA, 3, NA)
w1 <- rep(1, 6)
w2 <- c(1, NA, 1, NA, 1, NA)

test_that("missing values give NA with na.rm = FALSE", {
  expect_identical(sdX(v, weights = w1), NA_real_)
  expect_identical(sdX(v, weights = w1, estimator = "ml"), NA_real_)
  expect_identical(sdX(v, weights = w2), NA_real_)
  expect_identical(sdX(1:3, weights = c(1, NA, 1)), NA_real_)
})


test_that("na.rm = TRUE drops incomplete (x, weight) pairs", {
  expect_equal(sdX(v, weights = w1, na.rm = TRUE), 1)
  expect_equal(sdX(v, weights = w2, na.rm = TRUE), 1)
  expect_equal(varX(1:5, weights = c(1, NA, 1, 1, 1), na.rm = TRUE),
               var(c(1, 3, 4, 5)))
  expect_equal(sdX(v, weights = c(1, 0, 1, 0, 1, 0), na.rm = TRUE), 1)
})
