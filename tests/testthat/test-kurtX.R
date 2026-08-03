test_that("kurt returns a single numeric", {
  x <- rnorm(100)
  res <- kurt(x)
  expect_length(res, 1)
  expect_true(is.numeric(res))
})

test_that("kurt is near 0 for a normal distribution (excess kurtosis, estimator=3)", {
  set.seed(1)
  x <- rnorm(5000)
  expect_lt(abs(kurt(x, estimator = 3)), 0.2)
})

test_that("kurt is higher for a leptokurtic distribution than normal", {
  set.seed(2)
  x_norm    <- rnorm(1000)
  x_lepto   <- rt(1000, df = 3)   # heavy tails
  expect_gt(kurt(x_lepto, estimator=3), kurt(x_norm, estimator=3))
})

test_that("kurt estimators 1, 2, 3 give different results", {
  set.seed(3)
  x  <- rnorm(50)
  k1 <- kurt(x, estimator = 1)
  k2 <- kurt(x, estimator = 2)
  k3 <- kurt(x, estimator = 3)
  expect_false(isTRUE(all.equal(k1, k2)))
  expect_false(isTRUE(all.equal(k1, k3)))
})

test_that("kurt stops for invalid estimator", {
  expect_error(kurt(rnorm(10), estimator = 4), "estimator")
})

test_that("kurt stops for non-numeric input", {
  expect_error(kurt(c("a","b","c")), "numeric")
})

test_that("kurt na.rm = TRUE strips NAs", {
  x <- c(rnorm(49), NA)
  expect_equal(kurt(x, na.rm = TRUE), kurt(x[1:49]))
})

test_that("kurt conf.level returns named vector with est/lci/uci", {
  set.seed(4)
  x <- rnorm(100)
  res <- kurt(x, conf.level = 0.95, R = 300)
  expect_length(res, 3)
  expect_true(all(c("est","lci","uci") %in% names(res)))
})

test_that("kurt CI: lci < est < uci", {
  set.seed(5)
  x <- rnorm(120)
  res <- kurt(x, conf.level = 0.95, R = 300)
  expect_lt(res["lci"], res["est"])
  expect_gt(res["uci"], res["est"])
})



test_that("the classic kurtosis interval is centred on the estimate", {
  
  set.seed(1)
  x <- rgamma(200, shape = 2)          # clearly leptokurtic, est well above 0
  
  res <- kurt(x, conf.level = 0.95, method = "classic")
  est <- kurt(x)
  
  expect_equal(unname(res[["est"]]), unname(est))
  
  # the former version returned -z*se and +z*se, an interval around ZERO
  # that need not even contain the estimate it is printed beside
  expect_lt(res[["lci"]], res[["est"]])
  expect_gt(res[["uci"]], res[["est"]])
  
  # and it is symmetric about the estimate
  expect_equal(res[["est"]] - res[["lci"]], res[["uci"]] - res[["est"]])
})


test_that("kurt honours na.rm on the bootstrap path", {
  
  set.seed(2)
  x <- c(rgamma(150, shape = 2), NA, NA)
  
  # x was never filtered and the resampling statistic ignored na.rm, so
  # every replicate came back NA and boot.ci() failed
  # type = "perc": my own tightened .extractBootArgs() now requires at
  # least 200 replicates for BCa, and R = 199 was chosen to keep the test
  # fast. Percentile has no such floor.
  expect_silent(res <- kurt(x, conf.level = 0.95, na.rm = TRUE,
                            R = 199, type = "perc"))
  expect_false(anyNA(res))
  expect_named(res, c("est", "lci", "uci"))
})


test_that("kurt pairs bootstrap values with their own weights", {
  
  set.seed(3)
  x <- c(rep(1, 40), rep(9, 10))
  w <- c(rep(1, 40), rep(5, 10))
  
  # weights were passed unsubset, so replicate i got the weight of the
  # ORIGINAL observation i rather than of the resampled one
  expect_silent(res <- kurt(x, weights = w, conf.level = 0.95,
                            R = 199, type = "perc"))
  expect_equal(unname(res[["est"]]), unname(kurt(x, weights = w)))
})


test_that("kurt returns an unnamed scalar without conf.level", {
  set.seed(4)
  expect_null(names(kurt(rnorm(50))))
})

