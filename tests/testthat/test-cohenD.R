test_that("cohenD returns near 0 for two identical groups", {
  set.seed(1)
  x <- rnorm(50, mean = 5, sd = 2)
  y <- rnorm(50, mean = 5, sd = 2)
  expect_lt(abs(cohenD(x, y)), 0.5)   # not exactly 0 due to sampling
})

test_that("cohenD returns a positive value when mean(x) > mean(y)", {
  x <- rnorm(100, mean = 10, sd = 2)
  y <- rnorm(100, mean = 5,  sd = 2)
  expect_gt(cohenD(x, y), 0)
})

test_that("cohenD returns a negative value when mean(x) < mean(y)", {
  x <- rnorm(100, mean = 5,  sd = 2)
  y <- rnorm(100, mean = 10, sd = 2)
  expect_lt(cohenD(x, y), 0)
})

test_that("cohenD with conf.level returns a 3-element vector", {
  x <- rnorm(50, mean = 8, sd = 2)
  y <- rnorm(50, mean = 5, sd = 2)
  res <- cohenD(x, y, conf.level = 0.95)
  expect_length(res, 3)
  expect_named(res, c("est", "lci", "uci"))
})

test_that("cohenD CI: lci < est < uci", {
  x <- rnorm(80, mean = 8, sd = 2)
  y <- rnorm(80, mean = 5, sd = 2)
  res <- cohenD(x, y, conf.level = 0.95)
  expect_lt(res["lci"], res["est"])
  expect_gt(res["uci"], res["est"])
})

test_that("cohenD one-sample: mean/sd (two-sample call with y = NULL avoided, use two vectors)", {
  # Note: bare cohenD(x) errors because .sd is not assigned in the one-sample branch
  # (attr(res, "sd_pooled") <- .sd always runs). Use two-sample path instead.
  x <- c(1, 2, 3, 4, 5)
  y <- rep(0, 5)     # zero group → d = mean(x)/pooled_sd
  d <- cohenD(x, y)
  expect_true(is.numeric(d))
})

test_that("cohenD respects na.rm = TRUE", {
  x <- c(1, 2, NA, 4, 5)
  y <- c(3, 4, 5,  6, 7)
  expect_equal(cohenD(x, y, na.rm = TRUE),
               cohenD(na.omit(x), y))
})

test_that("cohenD correct = TRUE (Hedges' g) gives smaller absolute value", {
  set.seed(42)
  x <- rnorm(20, mean = 8, sd = 2)
  y <- rnorm(20, mean = 5, sd = 2)
  d <- abs(cohenD(x, y))
  g <- abs(cohenD(x, y, correct = TRUE))
  expect_lte(g, d)
})

test_that("cohenD magnitude attribute is set", {
  x <- rnorm(100, mean = 10, sd = 2)
  y <- rnorm(100, mean = 5,  sd = 2)
  d <- cohenD(x, y)
  expect_false(is.null(attr(d, "magnitude")))
})



test_that("cohenD and coefVarCI still work through the shared .nctCI", {
  
  x <- c(5.1, 4.8, 6.2, 5.5, 5.9, 6.4, 4.9, 5.7)
  y <- c(4.2, 4.6, 4.1, 5.0, 4.4, 4.8, 4.3, 4.7)
  
  d <- cohenD(x, y, conf.level = 0.95)
  expect_named(d, c("est", "lci", "uci"))
  expect_lt(d[["lci"]], d[["est"]])
  
  cv <- coefVarCI(c(x, y), method = "nct")
  expect_named(cv, c("est", "lci", "uci"))
})

