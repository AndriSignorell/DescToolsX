
test_that("skew returns a single numeric (no CI)", {
  x <- rnorm(100)
  res <- skew(x)
  expect_length(res, 1); expect_true(is.numeric(res))
})

test_that("skew is near 0 for a normal distribution", {
  set.seed(1)
  x <- rnorm(5000)
  expect_lt(abs(skew(x, estimator = 3)), 0.1)
})

test_that("skew is positive for a right-skewed distribution", {
  x <- c(rep(1, 50), rep(10, 5))
  expect_gt(skew(x), 0)
})

test_that("skew is negative for a left-skewed distribution", {
  x <- c(rep(1, 5), rep(10, 50))
  expect_lt(skew(x), 0)
})

test_that("skew estimators 1, 2, 3 give different results for small n", {
  # Need asymmetric data — symmetric vectors give skew=0 for all estimators
  x  <- c(1, 1, 2, 3, 10, 15)
  s1 <- skew(x, estimator=1)
  s2 <- skew(x, estimator=2)
  s3 <- skew(x, estimator=3)
  expect_false(isTRUE(all.equal(s1, s2)))
  expect_false(isTRUE(all.equal(s1, s3)))
})

test_that("skew stops for invalid estimator", {
  expect_error(skew(rnorm(10), estimator = 4))
})

test_that("skew stops for non-numeric x", {
  expect_error(skew(c("a","b","c")), "numeric")
})

test_that("skew na.rm = TRUE strips NAs", {
  x <- c(rnorm(49), NA)
  expect_equal(skew(x, na.rm = TRUE), skew(x[1:49]))
})

test_that("skew conf.level = 0.95 returns named vector est/lci/uci", {
  set.seed(2)
  x <- rnorm(100)
  res <- skew(x, conf.level = 0.95, R = 300)
  expect_length(res, 3)
  expect_true(all(c("est","lci","uci") %in% names(res)))
})

test_that("skew CI: lci < est < uci", {
  set.seed(3)
  x <- c(rnorm(100), rep(5,10))
  res <- skew(x, conf.level = 0.95, R = 300)
  expect_lt(res["lci"], res["est"])
  expect_gt(res["uci"], res["est"])
})
