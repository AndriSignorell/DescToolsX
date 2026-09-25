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



# Review 25.09.2026 ------------------------------------------------------------

test_that("cStat counts concordant pairs, ties in x by half", {
  resp <- c(0, 0, 1, 1)
  expect_equal(cStat(c(0.1, 0.4, 0.35, 0.8), resp = resp), 3 / 4)
  expect_equal(cStat(c(0.1, 0.4, 0.4, 0.8), resp = resp), 3.5 / 4)
})

test_that("the glm method uses fitted probabilities and the response", {
  fit <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_equal(cStat(fit), cStat(fitted(fit), resp = mtcars$vs))
  # without the stored response the model frame is used
  fit0 <- glm(vs ~ mpg, data = mtcars, family = binomial, y = FALSE)
  expect_equal(cStat(fit0), cStat(fit))
})

test_that("cStat rejects missing, non-binary and invalid input", {
  expect_error(cStat(c(0.1, NA, 0.3), resp = c(0, 1, 1)), "missing values")
  expect_error(cStat(1:6, resp = rep(1:3, 2)), "binary")
  x <- runif(20)
  y <- rep(0:1, 10)
  for (cl in list(NULL, c(0.9, 0.95), NaN, 1.5, "0.95"))
    expect_error(cStat(x, resp = y, conf.level = cl))
})


test_that("sides opens the named side at the range boundary", {
  set.seed(9)
  x <- runif(200)
  y <- rbinom(200, 1, plogis(2 * x - 1))

  # R = 500 on purpose: 2 * 0.95 - 1 and 0.90 differ in the last bit, and
  # alpha/2 * R lands on 24.99999... for one and 25.00000... for the
  # other. cstat_boot_cpp() used to truncate that to adjacent order
  # statistics; the index rule must absorb the representation error.
  set.seed(1); two <- cStat(x, resp = y, conf.level = 0.90, R = 500)
  set.seed(1); l   <- cStat(x, resp = y, conf.level = 0.95, sides = "left",
                            R = 500)
  set.seed(1); r   <- cStat(x, resp = y, conf.level = 0.95, sides = "right",
                            R = 500)

  expect_named(l, c("est", "lci", "uci"))
  # same seed, same replicates: the one-sided bound is the two-sided one
  # at 2 * conf.level - 1
  expect_equal(unname(l[["lci"]]), unname(two[["lci"]]))
  expect_equal(unname(r[["uci"]]), unname(two[["uci"]]))
  expect_equal(unname(l[["uci"]]), 1)
  expect_equal(unname(r[["lci"]]), 0)
})

test_that("cStat validates sides and R", {
  x <- runif(20)
  y <- rep(0:1, 10)
  expect_error(cStat(x, resp = y, conf.level = 0.4, sides = "left"),
               "exceed 0.5")
  expect_error(cStat(x, resp = y, sides = "up"))
  expect_error(cStat(x, resp = y, conf.level = 0.95, R = 0), "'R'")
  expect_error(cStat(x, resp = y, conf.level = 0.95, R = 99.5), "'R'")
})

test_that("the glm method passes sides on", {
  fit <- glm(vs ~ mpg, data = mtcars, family = binomial)
  set.seed(2)
  res <- cStat(fit, conf.level = 0.95, sides = "right", R = 300)
  expect_equal(unname(res[["lci"]]), 0)
})
