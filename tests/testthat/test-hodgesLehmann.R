test_that("hodgesLehmann returns a single numeric (one-sample)", {
  x <- c(2, 4, 6, 8, 10)
  res <- hodgesLehmann(x)
  expect_length(res, 1)
  expect_true(is.numeric(res))
})

test_that("hodgesLehmann equals the median for a symmetric distribution", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(hodgesLehmann(x), 3, tolerance = 1e-6)
})

test_that("hodgesLehmann one-sample matches wilcox.test pseudomedian (no ties)", {
  set.seed(1)
  x <- rnorm(30, mean = 5)
  hl  <- hodgesLehmann(x)
  wt  <- wilcox.test(x, conf.int = TRUE, exact = FALSE)$estimate
  expect_equal(hl, unname(wt), tolerance = 0.01)
})

test_that("hodgesLehmann two-sample returns a numeric", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(3, 4, 5, 6, 7)
  res <- hodgesLehmann(x, y)
  expect_length(res, 1)
  expect_true(is.numeric(res))
})

test_that("hodgesLehmann two-sample shift = 0 when groups are identical", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(hodgesLehmann(x, x), 0, tolerance = 1e-9)
})

test_that("hodgesLehmann returns NA for NA input without na.rm", {
  expect_true(is.na(hodgesLehmann(c(1, 2, NA))))
})

test_that("hodgesLehmann na.rm = TRUE strips NAs before estimation", {
  x <- c(1, 2, 3, NA, 5)
  res <- hodgesLehmann(x, na.rm = TRUE)
  expect_false(is.na(res))
})

test_that("hodgesLehmann conf.level returns named vector est/lci/uci", {
  set.seed(42)
  x <- rnorm(50, mean = 5)
  res <- hodgesLehmann(x, conf.level = 0.95, R = 300)
  expect_length(res, 3)
  expect_named(res, c("est", "lci", "uci"))
})

test_that("hodgesLehmann CI: lci < est < uci", {
  set.seed(42)
  x <- rnorm(50, mean = 5)
  res <- hodgesLehmann(x, conf.level = 0.95, R = 300)
  expect_lt(res["lci"], res["est"])
  expect_gt(res["uci"], res["est"])
})

test_that("hodgesLehmann stops for non-numeric x", {
  expect_error(hodgesLehmann(c("a", "b")), "numeric")
})
