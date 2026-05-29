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
