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

test_that("boxCoxLambda defaults lower to 0 when series contains non-positives", {
  x <- c(0.1, 1, 2, 3, 4, 5)
  # Should not error and lambda >= 0
  lambda <- boxCoxLambda(ts(x))
  expect_gte(lambda, 0)
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
