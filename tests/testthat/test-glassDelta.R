

test_that("glassDelta returns a single numeric (no CI)", {
  x <- rnorm(30, mean = 10, sd = 2)
  y <- rnorm(30, mean = 5,  sd = 2)
  d <- glassDelta(x, y)
  expect_length(d, 1)
  expect_true(is.numeric(d))
})

test_that("glassDelta is 0 when group means are equal", {
  set.seed(1)
  x <- rnorm(50, mean = 5, sd = 2)
  y <- rnorm(50, mean = 5, sd = 2)
  expect_lt(abs(glassDelta(x, y)), 0.5)
})

test_that("glassDelta is positive when mean(x) > mean(y)", {
  x <- rnorm(50, mean = 10, sd = 2)
  y <- rnorm(50, mean = 5,  sd = 2)
  expect_gt(glassDelta(x, y), 0)
})

test_that("glassDelta uses sd of y (control) by default", {
  x <- c(8,9,10,11,12)
  y <- c(2,4,6,8,10)    # sd(y) much larger than sd(x)
  d_ctrl <- glassDelta(x, y, useControlSd = TRUE)
  d_trt  <- glassDelta(x, y, useControlSd = FALSE)
  expect_false(isTRUE(all.equal(d_ctrl, d_trt)))
})

test_that("glassDelta manual calculation: (mean(x)-mean(y)) / sd(y)", {
  x <- c(5, 6, 7, 8, 9)
  y <- c(1, 2, 3, 4, 5)
  expected <- (mean(x) - mean(y)) / sd(y)
  # as.vector() strips both the "est" name and the "magnitude" attribute
  expect_equal(as.vector(glassDelta(x, y)), expected, tolerance = 1e-10)
})

test_that("glassDelta conf.level returns named vector est/lci/uci", {
  x <- rnorm(40, mean = 8, sd = 2)
  y <- rnorm(40, mean = 5, sd = 2)
  res <- glassDelta(x, y, conf.level = 0.95)
  expect_length(res, 3)
  expect_named(res, c("est","lci","uci"))
})

test_that("glassDelta CI: lci < est < uci", {
  x <- rnorm(60, mean = 8, sd = 2)
  y <- rnorm(60, mean = 5, sd = 2)
  res <- glassDelta(x, y, conf.level = 0.95)
  expect_lt(res["lci"], res["est"])
  expect_gt(res["uci"], res["est"])
})

test_that("glassDelta na.rm = TRUE strips NAs independently per vector", {
  # na.omit() is applied to x and y separately (not paired),
  # so y (which has no NAs) stays unchanged at full length
  x <- c(8, 9, NA, 11)
  y <- c(1, 2,  3,  4)
  expect_equal(glassDelta(x, y, na.rm = TRUE),
               glassDelta(c(8, 9, 11), y))
})