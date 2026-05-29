
test_that("skewX returns a single numeric (no CI)", {
  x <- rnorm(100)
  res <- skewX(x)
  expect_length(res, 1); expect_true(is.numeric(res))
})

test_that("skewX is near 0 for a normal distribution", {
  set.seed(1)
  x <- rnorm(5000)
  expect_lt(abs(skewX(x, estimator = 3)), 0.1)
})

test_that("skewX is positive for a right-skewed distribution", {
  x <- c(rep(1, 50), rep(10, 5))
  expect_gt(skewX(x), 0)
})

test_that("skewX is negative for a left-skewed distribution", {
  x <- c(rep(1, 5), rep(10, 50))
  expect_lt(skewX(x), 0)
})

test_that("skewX estimators 1, 2, 3 give different results for small n", {
  # Need asymmetric data — symmetric vectors give skew=0 for all estimators
  x  <- c(1, 1, 2, 3, 10, 15)
  s1 <- skewX(x, estimator=1)
  s2 <- skewX(x, estimator=2)
  s3 <- skewX(x, estimator=3)
  expect_false(isTRUE(all.equal(s1, s2)))
  expect_false(isTRUE(all.equal(s1, s3)))
})

test_that("skewX stops for invalid estimator", {
  expect_error(skewX(rnorm(10), estimator = 4))
})

test_that("skewX stops for non-numeric x", {
  expect_error(skewX(c("a","b","c")), "numeric")
})

test_that("skewX na.rm = TRUE strips NAs", {
  x <- c(rnorm(49), NA)
  expect_equal(skewX(x, na.rm = TRUE), skewX(x[1:49]))
})

test_that("skewX conf.level = 0.95 returns named vector est/lci/uci", {
  set.seed(2)
  x <- rnorm(100)
  res <- skewX(x, conf.level = 0.95, R = 300)
  expect_length(res, 3)
  expect_true(all(c("est","lci","uci") %in% names(res)))
})

test_that("skewX CI: lci < est < uci", {
  set.seed(3)
  x <- c(rnorm(100), rep(5,10))
  res <- skewX(x, conf.level = 0.95, R = 300)
  expect_lt(res["lci"], res["est"])
  expect_gt(res["uci"], res["est"])
})
