test_that("cStat returns a value in [0, 1]", {
  set.seed(1)
  x <- runif(100)
  y <- rbinom(100, 1, 0.5)
  c_val <- cStat(x, resp = y)
  expect_gte(c_val, 0)
  expect_lte(c_val, 1)
})

test_that("cStat is near 1 for a perfect predictor", {
  y <- c(rep(0, 50), rep(1, 50))
  x <- c(rep(0.1, 50), rep(0.9, 50))   # perfect separation
  expect_gt(cStat(x, resp = y), 0.99)
})

test_that("cStat is near 0.5 for a random predictor", {
  set.seed(42)
  y <- rbinom(1000, 1, 0.5)
  x <- runif(1000)
  c_val <- cStat(x, resp = y)
  expect_lt(abs(c_val - 0.5), 0.1)
})

test_that("cStat + (1 - cStat) with flipped predictor ≈ 1", {
  set.seed(3)
  y <- rbinom(200, 1, 0.5)
  x <- runif(200)
  expect_equal(unname(cStat(x, resp = y)) + unname(cStat(1 - x, resp = y)),
               1, tolerance = 0.01)
})

test_that("cStat with conf.level returns named vector est/lci/uci", {
  set.seed(4)
  x <- runif(200)
  y <- rbinom(200, 1, plogis(2 * x - 1))
  res <- cStat(x, resp = y, conf.level = 0.95, R = 300)
  expect_length(res, 3)
  # names may be "est.est"/"lci.lci"/"uci.uci" due to c() name-joining — strip suffix
  expect_true(all(grepl("est|lci|uci", names(res))))
})

test_that("cStat CI: lci < est < uci", {
  set.seed(5)
  x <- runif(300)
  y <- rbinom(300, 1, plogis(2 * x - 1))
  res <- cStat(x, resp = y, conf.level = 0.95, R = 300)
  res <- unname(res)
  expect_lt(res[2], res[1])
  expect_gt(res[3], res[1])
})

test_that("cStat accepts a logical response vector", {
  set.seed(6)
  x <- runif(100)
  y <- x > 0.5
  c_val <- cStat(x, resp = y)
  expect_gte(c_val, 0)
  expect_lte(c_val, 1)
})

test_that("cStat stops when x and resp have different lengths", {
  expect_error(cStat(1:10, resp = 1:5), "same length")
})

test_that("cStat stops when resp is missing in default method", {
  expect_error(cStat(1:10), "resp")
})




test_that("cStat returns an unnamed scalar and refuses a constant response", {
  
  set.seed(7)
  x <- runif(100)
  y <- rbinom(100, 1, 0.5)
  
  est <- cStat(x, resp = y)
  expect_null(names(est))
  expect_true(est >= 0 && est <= 1)
  
  expect_error(cStat(x, resp = rep(1, 100)), "both outcome classes")
})


test_that("cStat reports the same estimate with and without an interval", {
  
  set.seed(8)
  x <- runif(200)
  y <- rbinom(200, 1, plogis(2 * x - 1))
  
  plain <- cStat(x, resp = y)
  withCi <- cStat(x, resp = y, conf.level = 0.95)
  
  expect_equal(unname(withCi[["est"]]), unname(plain))
})

