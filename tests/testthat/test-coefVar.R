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


# Additional branch coverage and reference checks
test_that("coefVar applies the documented finite-sample correction exactly", {
  x <- c(8, 9, 10, 11, 12)
  K <- sd(x) / mean(x)
  n <- length(x)
  expected <- K * (1 - 1 / (4 * (n - 1)) + K^2 / n + 1 / (2 * (n - 1)^2))
  expect_equal(coefVar(x, unbiased = TRUE), expected)
  expect_warning(ans <- coefVar(c(-1, 1, 1)), "exceeds 1")
  expect_equal(ans, sd(c(-1, 1, 1)) / mean(c(-1, 1, 1)))
})

test_that("coefVar lm and aov methods use residual degrees of freedom", {
  d <- data.frame(y = c(8, 10, 9, 12, 11, 13), g = factor(rep(1:2, each = 3)))
  fit <- lm(y ~ g, d)
  K <- sqrt(sum(residuals(fit)^2) / df.residual(fit)) / mean(d$y)
  n <- df.residual(fit)
  expected <- K * (1 - 1 / (4 * (n - 1)) + K^2 / n + 1 / (2 * (n - 1)^2))
  expect_equal(coefVar(fit), K)
  expect_equal(coefVar(fit, unbiased = TRUE), expected)
  expect_equal(coefVar(aov(y ~ g, d), unbiased = TRUE), expected)
})

test_that("coefVarCI validates input and vectorizes confidence levels", {
  x <- c(8, 9, 10, 11, 12)
  expect_error(coefVarCI(matrix(x)), "numeric vector")
  expect_error(coefVarCI(letters), "numeric vector")
  for (bad in list("0.95", NA_real_, 0, 1, Inf))
    expect_error(coefVarCI(x, conf.level = bad), "conf.level")
  ans <- coefVarCI(x, conf.level = c(0.8, 0.95), method = "naive")
  expect_equal(dim(ans), c(2L, 3L))
  for (i in 1:2)
    expect_equal(ans[i, ], coefVarCI(x, conf.level = c(0.8, 0.95)[i], method = "naive"))
  expect_equal(coefVarCI(c(x, NA), na.rm = TRUE, method = "naive"),
               coefVarCI(x, method = "naive"))
})

test_that("coefVarCI one-sided naive limits have the correct quantiles", {
  x <- c(8, 9, 10, 11, 12)
  K <- sd(x) / mean(x)
  left <- coefVarCI(x, method = "naive", sides = "left")
  right <- coefVarCI(x, method = "naive", sides = "right")
  expect_equal(left, c(est = K, lci = K * sqrt(4 / qchisq(0.95, 4)), uci = Inf))
  expect_equal(right, c(est = K, lci = -Inf, uci = K * sqrt(4 / qchisq(0.05, 4))))
})

test_that("coefVar forwards frequency weights consistently to point and interval estimates", {
  x <- c(8, 10, 13)
  w <- c(2, 3, 1)
  ref <- sdX(x, weights = w) / weighted.mean(x, w)
  expect_equal(coefVar(x, weights = w), ref)
  n <- sum(w)
  expect_equal(coefVarCI(x, weights = w, method = "naive"),
               c(est = ref, lci = ref * sqrt((n - 1) / qchisq(0.975, n - 1)),
                 uci = ref * sqrt((n - 1) / qchisq(0.025, n - 1))))
})
