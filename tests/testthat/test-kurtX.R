test_that("kurtX returns a single numeric", {
  x <- rnorm(100)
  res <- kurtX(x)
  expect_length(res, 1)
  expect_true(is.numeric(res))
})

test_that("kurtX is near 0 for a normal distribution (excess kurtosis, estimator=3)", {
  set.seed(1)
  x <- rnorm(5000)
  expect_lt(abs(kurtX(x, estimator = 3)), 0.2)
})

test_that("kurtX is higher for a leptokurtic distribution than normal", {
  set.seed(2)
  x_norm    <- rnorm(1000)
  x_lepto   <- rt(1000, df = 3)   # heavy tails
  expect_gt(kurtX(x_lepto, estimator=3), kurtX(x_norm, estimator=3))
})

test_that("kurtX estimators 1, 2, 3 give different results", {
  set.seed(3)
  x  <- rnorm(50)
  k1 <- kurtX(x, estimator = 1)
  k2 <- kurtX(x, estimator = 2)
  k3 <- kurtX(x, estimator = 3)
  expect_false(isTRUE(all.equal(k1, k2)))
  expect_false(isTRUE(all.equal(k1, k3)))
})

test_that("kurtX stops for invalid estimator", {
  expect_error(kurtX(rnorm(10), estimator = 4), "estimator")
})

test_that("kurtX stops for non-numeric input", {
  expect_error(kurtX(c("a","b","c")), "numeric")
})

test_that("kurtX na.rm = TRUE strips NAs", {
  x <- c(rnorm(49), NA)
  expect_equal(kurtX(x, na.rm = TRUE), kurtX(x[1:49]))
})

test_that("kurtX conf.level returns named vector with est/lci/uci", {
  set.seed(4)
  x <- rnorm(100)
  res <- kurtX(x, conf.level = 0.95, R = 300)
  expect_length(res, 3)
  expect_true(all(c("est","lci","uci") %in% names(res)))
})

test_that("kurtX CI: lci < est < uci", {
  set.seed(5)
  x <- rnorm(120)
  res <- kurtX(x, conf.level = 0.95, R = 300)
  expect_lt(res["lci"], res["est"])
  expect_gt(res["uci"], res["est"])
})
