test_that("hmean returns the harmonic mean for positive values", {
  x <- c(1, 2, 4)
  expected <- 1 / mean(1/x)
  expect_equal(hmean(x), expected, tolerance = 1e-10)
})

test_that("hmean equals arithmetic mean for a constant vector", {
  expect_equal(hmean(rep(5, 10)), 5)
})

test_that("hmean <= geometric mean <= arithmetic mean", {
  x <- c(1, 2, 4, 8)
  h <- hmean(x)
  g <- exp(mean(log(x)))
  a <- mean(x)
  expect_lte(h, g + 1e-9)
  expect_lte(g, a + 1e-9)
})

test_that("hmean treats zero and negative values as NA (silently)", {
  x <- c(1, 2, 0, 4)
  res <- hmean(x, na.rm = TRUE)
  expect_true(is.numeric(res))
})

test_that("hmean returns NA without na.rm when NAs present", {
  x <- c(1, 2, NA, 4)
  expect_true(is.na(hmean(x)))
})

test_that("hmean na.rm = TRUE gives same result as dropping NAs", {
  x <- c(2, 4, NA, 8)
  expect_equal(hmean(x, na.rm = TRUE), hmean(c(2, 4, 8)))
})

test_that("hmean conf.level returns a 3-element vector", {
  x <- c(1, 2, 4, 8, 16)
  res <- hmean(x, conf.level = 0.95)
  expect_length(res, 3)
})

test_that("hmean CI: lci < estimate < uci (for positive data)", {
  set.seed(7)
  x <- runif(80, 1, 10)
  res <- hmean(x, conf.level = 0.95)
  expect_lt(res[2], res[1])
  expect_gt(res[3], res[1])
})
