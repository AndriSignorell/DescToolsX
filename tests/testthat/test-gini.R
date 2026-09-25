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
  # type = "perc": the bca adjustment pushes the quantiles of R = 300
  # replicates of n = 4 onto the extreme order statistics and warns
  res <- gini(x, conf.level = 0.95, R = 300, type = "perc")
  expect_length(res, 3)
  expect_named(res, c("est", "lci", "uci"))
})

test_that("gini CI: lci < est < uci", {
  x <- c(10, 20, 30, 40, 50, 60)
  res <- gini(x, conf.level = 0.95, R = 300, type = "perc")
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


# Review 25.09.2026 ------------------------------------------------------------

test_that("weights are frequency weights, as the replicated vector shows", {
  expect_equal(gini(c(10, 0), weights = c(2, 3)), gini(c(0, 0, 0, 10, 10)))
  expect_equal(gini(c(10, 0), weights = c(2, 3), unbiased = FALSE),
               gini(c(0, 0, 0, 10, 10), unbiased = FALSE))
})

test_that("one-sided intervals open at the range boundary", {
  x <- c(3, 8, 1, 12, 5, 7, 2, 9, 4, 15)
  set.seed(5)
  two <- gini(x, conf.level = 0.90, R = 300, type = "perc")
  set.seed(5)
  l <- gini(x, conf.level = 0.95, sides = "left", R = 300, type = "perc")
  set.seed(5)
  r <- gini(x, conf.level = 0.95, sides = "right", R = 300, type = "perc")
  expect_equal(unname(l[["lci"]]), unname(two[["lci"]]))
  expect_equal(unname(r[["uci"]]), unname(two[["uci"]]))
  expect_equal(unname(l[["uci"]]), 1)
  expect_equal(unname(r[["lci"]]), 0)
  expect_error(gini(x, conf.level = 0.4, sides = "left"), "exceed 0.5")
})

test_that("zero total weight gives NA of the requested shape", {
  expect_true(is.na(gini(c(1, 2), weights = c(0, 0))))
  res <- gini(c(1, 2), weights = c(0, 0), conf.level = 0.95)
  expect_named(res, c("est", "lci", "uci"))
  expect_true(all(is.na(res)))
})

test_that("gini validates its arguments", {
  expect_error(gini(letters), "numeric")
  expect_error(gini(1:3, weights = 1:2), "same length")
  expect_error(gini(c(1, Inf, 3)), "finite")
  expect_error(gini(1:3, unbiased = NA), "unbiased")
  expect_error(gini(1:3, na.rm = "yes"), "na.rm")
  for (cl in list(NULL, NaN, c(0.9, 0.95)))
    expect_error(gini(1:3, conf.level = cl), "conf.level")
  expect_error(gini(1:3, sides = "up"))
  expect_error(gini(c(1, 2), weights = c(0.5, 0.5)), "effective sample size")
  expect_equal(gini(c(1, NA, 3), na.rm = TRUE), gini(c(1, 3)))
})
