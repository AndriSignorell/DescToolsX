test_that("boxCoxLambda returns a single numeric value (guerrero)", {
  lambda <- boxCoxLambda(AirPassengers)
  expect_length(lambda, 1)
  expect_true(is.numeric(lambda))
})

test_that("boxCoxLambda result is within [lower, upper] bounds (guerrero)", {
  lambda <- boxCoxLambda(AirPassengers, lower = -1, upper = 2)
  expect_gte(lambda, -1)
  expect_lte(lambda, 2)
})

test_that("boxCoxLambda returns a numeric value for loglik method", {
  lambda <- boxCoxLambda(AirPassengers, method = "loglik")
  expect_length(lambda, 1)
  expect_true(is.numeric(lambda))
})

test_that("boxCoxLambda loglik result is within [lower, upper]", {
  lambda <- boxCoxLambda(AirPassengers, method = "loglik", lower = -1, upper = 2)
  expect_gte(lambda, -1)
  expect_lte(lambda, 2)
})

test_that("boxCoxLambda rejects non-positive series instead of clamping", {
  # an earlier version clamped 'lower' to 0 for such input, which still
  # needs log(x) and is just as undefined
  expect_error(boxCoxLambda(ts(c(0, 1, 2, 3, 4, 5))), "must be positive")
  expect_error(boxCoxLambda(c(-1, 1, 2, 3)), "must be positive")
})

test_that("boxCoxLambda is consistent: same input gives same output", {
  set.seed(42)
  x <- ts(abs(rnorm(60)) + 1)
  expect_equal(boxCoxLambda(x), boxCoxLambda(x))
})

test_that("boxCoxLambda accepts custom lower and upper bounds", {
  lambda <- boxCoxLambda(AirPassengers, lower = 0, upper = 1)
  expect_gte(lambda, 0)
  expect_lte(lambda, 1)
})


# Review 25.09.2026 ------------------------------------------------------------

test_that("loglik maximises the profile likelihood of MASS::boxcox", {
  skip_if_not_installed("MASS")
  set.seed(5)
  x <- exp(seq(1, 3, length.out = 40) + rnorm(40, sd = 0.1))
  trend <- seq_along(x)
  grid <- seq(-1, 2, by = 0.001)
  bc <- MASS::boxcox(x ~ trend, lambda = grid, plotit = FALSE)
  # absolute difference: lambda is close to 0 here, where expect_equal()'s
  # relative tolerance would be meaningless
  expect_lt(abs(boxCoxLambda(x, method = "loglik") - bc$x[which.max(bc$y)]),
            0.002)
})

test_that("the log branch near lambda = 0 is continuous with the power branch", {
  set.seed(6)
  x <- exp(seq(1, 3, length.out = 40) + rnorm(40, sd = 0.1))
  near0 <- boxCoxLambda(x, method = "loglik", lower = -0.01, upper = 0.01)
  expect_gte(near0, -0.01)
  expect_lte(near0, 0.01)
})

test_that("seasonality is taken from the ts frequency", {
  # a monthly series is fitted with seasonal dummies / monthly subseries
  x <- AirPassengers
  plain <- as.numeric(x)
  expect_false(isTRUE(all.equal(boxCoxLambda(x), boxCoxLambda(plain))))
  expect_false(isTRUE(all.equal(boxCoxLambda(x, method = "loglik"),
                                boxCoxLambda(plain, method = "loglik"))))
  # the seasonal loglik model needs more than frequency + 1 observations
  expect_error(boxCoxLambda(ts(1:13 + 0.5, frequency = 12), method = "loglik"),
               "needs more than 13")
  expect_error(boxCoxLambda(ts(1:23 + 0.5, frequency = 12)),
               "too short for method \"guerrero\"")
})

test_that("guerrero refuses subseries without variation", {
  expect_error(boxCoxLambda(c(1, 1, 2, 2, 3, 3)), "no variation")
})

test_that("boxCoxLambda() validates its input", {
  expect_error(boxCoxLambda("a"), "numeric vector")
  expect_error(boxCoxLambda(matrix(1:4, 2) + 0.5), "numeric vector")
  expect_error(boxCoxLambda(c(1, NA, 3, 4)), "non-finite")
  expect_error(boxCoxLambda(c(1, Inf, 3, 4)), "non-finite")
  expect_error(boxCoxLambda(c(2, 3), method = "loglik"), "at least 3")
  expect_error(boxCoxLambda(2), "at least 2")
  expect_error(boxCoxLambda(rep(4, 10)), "constant")
  expect_error(boxCoxLambda(AirPassengers, lower = 1, upper = 1), "strictly less")
  expect_error(boxCoxLambda(AirPassengers, lower = NA), "'lower'")
  expect_error(boxCoxLambda(AirPassengers, upper = c(1, 2)), "'upper'")
  for (nl in list(1, 2.5, NA, "3"))
    expect_error(boxCoxLambda(c(1, 3, 2, 5, 4, 6), nonseasonalLength = nl),
                 "nonseasonalLength")
})
