test_that("gini returns 0 for a perfectly equal distribution", {
  expect_equal(gini(rep(10, 100)), 0)
})

test_that("gini returns (n-1)/n for maximal inequality (one person has everything)", {
  # Theoretical max for finite n is (n-1)/n, not 1 (that is only the limit as n → Inf)
  x <- c(rep(0, 9), 100)
  expect_equal(gini(x, unbiased = FALSE), (length(x) - 1) / length(x),
               tolerance = 1e-10)
})

test_that("gini result is in [0, 1] for typical data", {
  x <- c(10, 20, 30, 40)
  g <- gini(x)
  expect_gte(g, 0); expect_lte(g, 1)
})

test_that("gini increases with greater inequality", {
  eq   <- gini(rep(10, 4))
  ineq <- gini(c(1, 1, 1, 100))
  expect_lt(eq, ineq)
})

test_that("gini unbiased = FALSE gives lower value than unbiased = TRUE", {
  x <- c(10, 20, 30, 40)
  expect_lt(gini(x, unbiased = FALSE), gini(x, unbiased = TRUE))
})

test_that("gini na.rm = TRUE strips NAs", {
  x <- c(10, 20, NA, 40)
  expect_equal(gini(x, na.rm = TRUE), gini(c(10, 20, 40)))
})

test_that("gini stops for negative values", {
  expect_error(gini(c(1, -2, 3)))
})

test_that("gini stops for NA without na.rm", {
  expect_error(gini(c(1, NA, 3)))
})

test_that("gini conf.level returns named vector est/lci/uci", {
  x <- c(10, 20, 30, 40)
  res <- gini(x, conf.level = 0.95, R = 300)
  expect_length(res, 3)
  expect_named(res, c("est", "lci", "uci"))
})

test_that("gini CI: lci < est < uci", {
  x <- c(10, 20, 30, 40, 50, 60)
  res <- gini(x, conf.level = 0.95, R = 300)
  expect_lt(res["lci"], res["est"])
  expect_gt(res["uci"], res["est"])
})

test_that("gini weighted: uniform weights give same result as unweighted", {
  x <- c(10, 20, 30, 40)
  expect_equal(gini(x, weights = rep(1, 4)), gini(x))
})



test_that("gini stays inside [0, 1] however the weights are expressed", {
  
  # frequency weights and the equivalent replicated vector must agree
  weighted <- gini(c(10, 0), weights = c(2, 3))
  replicated <- gini(c(0, 0, 0, 10, 10))
  
  expect_equal(weighted, replicated)
  expect_lte(weighted, 1)
  expect_gte(weighted, 0)
  
  # unweighted correction is unchanged: n/(n-1)
  x <- c(10, 20, 30, 40)
  expect_equal(gini(x, unbiased = TRUE),
               gini(x, unbiased = FALSE) * length(x) / (length(x) - 1))
})


test_that("gini honours sides", {
  
  set.seed(1)
  x <- rlnorm(60)
  
  two   <- gini(x, conf.level = 0.95, R = 299)
  left  <- gini(x, conf.level = 0.95, R = 299, sides = "left")
  right <- gini(x, conf.level = 0.95, R = 299, sides = "right")
  
  expect_equal(unname(left[["uci"]]), 1)
  expect_equal(unname(right[["lci"]]), 0)
  expect_true(is.finite(left[["lci"]]))
  expect_true(is.finite(right[["uci"]]))
  
  expect_gte(unname(two[["lci"]]), 0)
  expect_lte(unname(two[["uci"]]), 1)
})


