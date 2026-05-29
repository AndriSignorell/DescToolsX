test_that("coefVar returns sd/mean for a simple vector", {
  x <- c(2, 4, 6, 8, 10)
  expect_equal(coefVar(x), sd(x) / mean(x))
})

test_that("coefVar returns 0 for a constant vector", {
  expect_equal(coefVar(rep(5, 10)), 0)
})

test_that("coefVar with unbiased = TRUE applies the bias correction", {
  x <- rnorm(50, mean = 10, sd = 2)
  cv_raw  <- coefVar(x, unbiased = FALSE)
  cv_adj  <- coefVar(x, unbiased = TRUE)
  # Bias correction always increases the CV slightly for typical data
  expect_false(isTRUE(all.equal(cv_raw, cv_adj)))
})

test_that("coefVar respects na.rm = TRUE", {
  x <- c(2, 4, NA, 8, 10)
  expect_equal(coefVar(x, na.rm = TRUE),
               coefVar(na.omit(x)))
})

test_that("coefVar errors when NA present and na.rm = FALSE", {
  # meanX(x) returns NA → if(abs(NA) < 0.1) fails with 'missing value where TRUE/FALSE needed'
  x <- c(2, 4, NA, 8, 10)
  expect_error(coefVar(x))
})

test_that("coefVar.lm returns a numeric value for a linear model", {
  fit <- lm(Sepal.Length ~ Sepal.Width, data = iris)
  cv <- coefVar(fit)
  expect_length(cv, 1)
  expect_true(is.numeric(cv))
  expect_gt(cv, 0)
})

test_that("coefVarCI returns a 3-element vector with est, lci, uci", {
  x <- rnorm(100, mean = 10, sd = 2)
  ci <- coefVarCI(x)
  expect_length(ci, 3)
  expect_named(ci, c("est", "lci", "uci"))
})

test_that("coefVarCI lower CI < estimate < upper CI", {
  x <- rnorm(100, mean = 10, sd = 2)
  ci <- coefVarCI(x)
  expect_lt(ci["lci"], ci["est"])
  expect_gt(ci["uci"], ci["est"])
})

test_that("coefVarCI method = 'vangel' works", {
  x <- rnorm(100, mean = 10, sd = 2)
  ci <- coefVarCI(x, method = "vangel")
  expect_length(ci, 3)
})

test_that("coefVarCI method = 'mckay' works", {
  x <- rnorm(100, mean = 10, sd = 2)
  ci <- coefVarCI(x, method = "mckay")
  expect_length(ci, 3)
})

test_that("coefVarCI method = 'naive' works", {
  x <- rnorm(100, mean = 10, sd = 2)
  ci <- coefVarCI(x, method = "naive")
  expect_length(ci, 3)
})
