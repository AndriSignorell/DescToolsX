test_that("ccc returns a list with the expected components", {
  x <- 1:20
  y <- 1:20
  res <- ccc(x, y)
  expect_type(res, "list")
  expect_named(res, c("rho.c", "s.shift", "l.shift", "C.b", "blalt"))
})

test_that("ccc rho.c is near 1 for perfect agreement", {
  x <- 1:50
  y <- 1:50
  res <- ccc(x, y)
  expect_gt(res$rho.c$est, 0.999)
})

test_that("ccc rho.c is a data.frame with columns est, lci, uci", {
  x <- rnorm(40, 5, 2)
  y <- x + rnorm(40, 0, 0.5)
  res <- ccc(x, y)
  expect_s3_class(res$rho.c, "data.frame")
  expect_named(res$rho.c, c("est", "lci", "uci"))
})

test_that("ccc rho.c CI: lci < est < uci", {
  x <- rnorm(60, 5, 2)
  y <- x + rnorm(60, 0, 1)
  res <- ccc(x, y)
  expect_lt(res$rho.c$lci, res$rho.c$est)
  expect_gt(res$rho.c$uci, res$rho.c$est)
})

test_that("ccc rho.c is near 0 for uncorrelated variables", {
  set.seed(7)
  x <- rnorm(200)
  y <- rnorm(200)
  res <- ccc(x, y)
  expect_lt(abs(res$rho.c$est), 0.2)
})

test_that("ccc C.b equals 1 for perfect concordance (x == y)", {
  x <- 1:30
  expect_equal(ccc(x, x)$C.b, 1, tolerance = 1e-6)
})

test_that("ccc blalt data frame has columns mean and delta", {
  x <- 1:10; y <- 1:10
  res <- ccc(x, y)
  expect_named(res$blalt, c("mean", "delta"))
  expect_equal(nrow(res$blalt), 10)
})

test_that("ccc na.rm = TRUE removes NA pairs before computation", {
  x <- c(1:10, NA)
  y <- c(1:10, 5)
  res_narm <- ccc(x, y, na.rm = TRUE)
  res_clean <- ccc(1:10, 1:10)
  expect_equal(res_narm$rho.c$est, res_clean$rho.c$est, tolerance = 1e-6)
})

test_that("ccc method asymptotic also returns valid result", {
  x <- rnorm(50, 5, 2)
  y <- x + rnorm(50, 0, 1)
  res <- ccc(x, y, ci = "asymptotic")
  expect_gte(res$rho.c$est, -1)
  expect_lte(res$rho.c$est,  1)
})
