test_that("atkinson returns 0 for a perfectly equal distribution", {
  x <- rep(10, 100)
  expect_equal(atkinson(x), 0)
})

test_that("atkinson returns 0 for a single positive value", {
  expect_equal(atkinson(42), 0)
})

test_that("atkinson is in [0, 1] for standard inputs", {
  x <- c(1, 2, 3, 4, 10, 20)
  a <- atkinson(x, epsilon = 0.5)
  expect_gte(a, 0)
  expect_lte(a, 1)
})

test_that("atkinson epsilon = 1 uses geometric-mean branch", {
  x <- c(1, 2, 4, 8)
  # both branches must return values in [0, 1]
  a1 <- atkinson(x, epsilon = 1)
  expect_gte(a1, 0)
  expect_lte(a1, 1)
})

test_that("atkinson epsilon = 0 gives 0 (no inequality aversion)", {
  x <- c(1, 2, 4, 8)
  expect_equal(atkinson(x, epsilon = 0), 0)
})

test_that("higher epsilon weights lower incomes more (more sensitive)", {
  x <- c(1, 5, 10, 50, 100)
  a_low  <- atkinson(x, epsilon = 0.2)
  a_high <- atkinson(x, epsilon = 2)
  expect_lt(a_low, a_high)
})

test_that("atkinson returns NA for negative values", {
  expect_true(is.na(atkinson(c(1, -1, 3))))
})

test_that("atkinson returns NA for negative epsilon", {
  expect_true(is.na(atkinson(c(1, 2, 3), epsilon = -0.5)))
})

test_that("atkinson handles NA values: na.rm = FALSE returns NA", {
  expect_true(is.na(atkinson(c(1, 2, NA))))
})

test_that("atkinson handles NA values: na.rm = TRUE strips NAs", {
  x <- c(1, 2, 3, 4)
  expect_equal(atkinson(c(1, 2, 3, 4, NA), na.rm = TRUE),
               atkinson(x))
})

test_that("atkinson frequency weights n work correctly", {
  x <- c(1, 2)
  # n = c(2, 2): same as rep(c(1, 2), c(2, 2))
  expect_equal(atkinson(x, n = c(2, 2)),
               atkinson(rep(x, c(2, 2))))
})

test_that("atkinson returns 0 when mean is 0 (all-zero vector)", {
  expect_equal(atkinson(rep(0, 5)), 0)
})


# Additional coverage: explicit branches and reference results
test_that("atkinson validates argument types, lengths and domains", {
  expect_error(atkinson("1"), "numeric vector")
  for (bad in list(1, NA, logical(), c(TRUE, FALSE)))
    expect_error(atkinson(1:3, na.rm = bad), "'na.rm'")
  for (bad in list("1", numeric(), c(0.5, 1), NA_real_, NaN, Inf, -Inf))
    expect_error(atkinson(1:3, epsilon = bad), "'epsilon'")
  for (bad in list("0", numeric(), c(0, 1), NA_real_, Inf, -1))
    expect_error(atkinson(1:3, tol = bad), "'tol'")
  for (bad in list("1", NA_real_, Inf, -1, 0.5))
    expect_error(atkinson(1:3, n = bad), "whole numbers")
  expect_error(atkinson(1:3, n = c(1, 2)), "same length")
})

test_that("atkinson handles scalar, zero and removed frequencies", {
  expect_equal(atkinson(1:3, n = 4), atkinson(rep(1:3, each = 4)))
  expect_identical(atkinson(numeric()), NA_real_)
  expect_identical(atkinson(1:3, n = 0), NA_real_)
  expect_identical(atkinson(c(NA_real_, NaN), na.rm = TRUE), NA_real_)
  expect_identical(atkinson(c(1, Inf)), NA_real_)
  # Zero-frequency observations must not contaminate the result.
  expect_equal(atkinson(c(1, 4, NA, Inf, -1), n = c(1, 3, 0, 0, 0)),
               atkinson(c(1, 4), n = c(1, 3)))
  expect_equal(atkinson(c(1, NA, 4), n = c(1, 99, 3), na.rm = TRUE),
               atkinson(c(1, 4), n = c(1, 3)))
})

test_that("atkinson zero-income and tolerance branches have known values", {
  expect_equal(atkinson(c(0, 2), epsilon = 0.5), 0.5)
  expect_equal(atkinson(c(0, 2), epsilon = 1), 1)
  expect_equal(atkinson(c(0, 2), epsilon = 2), 1)
  expect_equal(atkinson(c(1, 4), epsilon = 1, tol = 0), 0.2)
  expect_equal(atkinson(c(1, 4), epsilon = 1 + 1e-9, tol = 1e-8), 0.2)
  expect_equal(atkinson(c(1, 4), epsilon = 2), 0.36)
  expect_equal(atkinson(c(1, 4), epsilon = 0.5), 0.1)
})

test_that("atkinson agrees with a replicated-sample reference", {
  x <- c(1, 3, 11)
  n <- c(4, 2, 1)
  z <- rep(x, n)
  for (e in c(0.5, 1, 2)) {
    equivalent <- if (e == 1) exp(mean(log(z))) else mean(z^(1 - e))^(1 / (1 - e))
    expect_equal(atkinson(x, n = n, epsilon = e), 1 - equivalent / mean(z))
    expect_equal(atkinson(x * 1e200, n = n, epsilon = e),
                 atkinson(x, n = n, epsilon = e))
  }
  # Both raw sums overflow, but the normalized calculation is finite.
  expect_equal(atkinson(c(1e308, 5e307), n = c(1e308, 1e308)),
               atkinson(c(2, 1)))
})
