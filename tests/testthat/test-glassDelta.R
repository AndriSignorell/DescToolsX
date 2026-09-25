

test_that("glassDelta returns a single numeric (no CI)", {
  x <- rnorm(30, mean = 10, sd = 2)
  y <- rnorm(30, mean = 5,  sd = 2)
  d <- glassDelta(x, y)
  expect_length(d, 1)
  expect_true(is.numeric(d))
})

test_that("glassDelta is 0 when group means are equal", {
  set.seed(1)
  x <- rnorm(50, mean = 5, sd = 2)
  y <- rnorm(50, mean = 5, sd = 2)
  expect_lt(abs(glassDelta(x, y)), 0.5)
})

test_that("glassDelta is positive when mean(x) > mean(y)", {
  x <- rnorm(50, mean = 10, sd = 2)
  y <- rnorm(50, mean = 5,  sd = 2)
  expect_gt(glassDelta(x, y), 0)
})

test_that("glassDelta uses sd of y (control) by default", {
  x <- c(8,9,10,11,12)
  y <- c(2,4,6,8,10)    # sd(y) much larger than sd(x)
  d_ctrl <- glassDelta(x, y, useControlSd = TRUE)
  d_trt  <- glassDelta(x, y, useControlSd = FALSE)
  expect_false(isTRUE(all.equal(d_ctrl, d_trt)))
})

test_that("glassDelta manual calculation: (mean(x)-mean(y)) / sd(y)", {
  x <- c(5, 6, 7, 8, 9)
  y <- c(1, 2, 3, 4, 5)
  expected <- (mean(x) - mean(y)) / sd(y)
  # as.vector() strips both the "est" name and the "magnitude" attribute
  expect_equal(as.vector(glassDelta(x, y)), expected, tolerance = 1e-10)
})

test_that("glassDelta conf.level returns named vector est/lci/uci", {
  x <- rnorm(40, mean = 8, sd = 2)
  y <- rnorm(40, mean = 5, sd = 2)
  res <- glassDelta(x, y, conf.level = 0.95)
  expect_length(res, 3)
  expect_named(res, c("est","lci","uci"))
})

test_that("glassDelta CI: lci < est < uci", {
  x <- rnorm(60, mean = 8, sd = 2)
  y <- rnorm(60, mean = 5, sd = 2)
  res <- glassDelta(x, y, conf.level = 0.95)
  expect_lt(res["lci"], res["est"])
  expect_gt(res["uci"], res["est"])
})

test_that("glassDelta na.rm = TRUE strips NAs independently per vector", {
  # na.omit() is applied to x and y separately (not paired),
  # so y (which has no NAs) stays unchanged at full length
  x <- c(8, 9, NA, 11)
  y <- c(1, 2,  3,  4)
  expect_equal(glassDelta(x, y, na.rm = TRUE),
               glassDelta(c(8, 9, 11), y))
})

# Review 25.09.2026 ------------------------------------------------------------

xg <- c(5.1, 6.3, 5.8, 7.0, 6.6, 5.9, 6.8, 7.4)
yg <- c(4.2, 4.9, 5.5, 4.1, 5.0, 4.6)

test_that("correct = TRUE applies the exact Hedges factor J(nC - 1)", {
  df <- length(yg) - 1
  J  <- gamma(df / 2) / (sqrt(df / 2) * gamma((df - 1) / 2))
  d  <- (mean(xg) - mean(yg)) / sd(yg)
  expect_equal(as.vector(glassDelta(xg, yg, correct = TRUE)), J * d)
  expect_lt(abs(as.vector(glassDelta(xg, yg, correct = TRUE))),
            abs(as.vector(glassDelta(xg, yg))))
})

test_that("the interval inverts the noncentral t distribution", {
  nC <- length(yg); nE <- length(xg)
  d  <- (mean(xg) - mean(yg)) / sd(yg)
  tObs <- d * sqrt(nC * nE / (nC + nE))
  # start next to tObs and let uniroot() extend only as far as needed
  # (pt() is decreasing in the ncp): a fixed bracket of +/-50 evaluated
  # pt() deep in the tail, where R warns that precision is lost
  ncp <- function(p) uniroot(function(nc) pt(tObs, nC - 1, nc) - p,
                             tObs + c(-1, 1), extendInt = "downX",
                             tol = 1e-12)$root
  scl <- sqrt((nC + nE) / (nC * nE))
  res <- glassDelta(xg, yg, conf.level = 0.9)
  expect_equal(unname(res[c("lci", "uci")]),
               c(ncp(0.95), ncp(0.05)) * scl, tolerance = 1e-4)
})

test_that("sides opens the named side and useControlSd switches the scale", {
  l <- glassDelta(xg, yg, conf.level = 0.95, sides = "left")
  r <- glassDelta(xg, yg, conf.level = 0.95, sides = "right")
  expect_true(is.finite(l[["lci"]]) && is.infinite(l[["uci"]]))
  expect_true(is.infinite(r[["lci"]]) && is.finite(r[["uci"]]))
  expect_equal(as.vector(glassDelta(xg, yg, useControlSd = FALSE)),
               (mean(xg) - mean(yg)) / sd(xg))
})

test_that("magnitude labels follow Cohen's thresholds", {
  lab <- function(d) attr(glassDelta(c(-1, 1) + d, c(-1, 1) / sqrt(2)),
                          "magnitude")
  expect_identical(lab(0.1), "negligible")
  expect_identical(lab(0.3), "small")
  expect_identical(lab(0.6), "medium")
  expect_identical(lab(1.0), "large")
})

test_that("missing values without na.rm give NA of the requested shape", {
  est <- glassDelta(c(xg, NA), yg)
  expect_length(est, 1L)
  expect_true(is.na(est))
  res <- glassDelta(c(xg, NA), yg, conf.level = 0.95)
  expect_named(res, c("est", "lci", "uci"))
  expect_true(all(is.na(res)))
  expect_true(is.na(attr(res, "magnitude")))
})

test_that("glassDelta() validates its input", {
  expect_error(glassDelta(letters, yg), "'x' must be a numeric")
  expect_error(glassDelta(xg, matrix(yg)), "'y' must be a numeric")
  expect_error(glassDelta(xg, yg, correct = NA))
  for (cl in list(NaN, "0.9", c(0.9, 0.95)))
    expect_error(glassDelta(xg, yg, conf.level = cl), "conf.level")
  expect_error(glassDelta(xg, yg, conf.level = 1), "conf.level")
  expect_error(glassDelta(c(xg, Inf), yg), "infinite")
  expect_error(glassDelta(xg, 1), "at least 2")
  expect_error(glassDelta(xg, rep(3, 4)), "finite and positive")
  expect_error(glassDelta(xg, yg[1:2], correct = TRUE), "at least 3")
})


test_that("a one-sided bound is the two-sided bound at 2 * conf.level - 1", {
  l   <- glassDelta(xg, yg, conf.level = 0.95, sides = "left")
  r   <- glassDelta(xg, yg, conf.level = 0.95, sides = "right")
  two <- glassDelta(xg, yg, conf.level = 0.90)
  expect_equal(unname(l[["lci"]]), unname(two[["lci"]]))
  expect_equal(unname(r[["uci"]]), unname(two[["uci"]]))
  expect_identical(unname(l[["uci"]]), Inf)
  expect_identical(unname(r[["lci"]]), -Inf)
  # the bias correction scales the bounds as well as the estimate
  lc <- glassDelta(xg, yg, conf.level = 0.95, sides = "left", correct = TRUE)
  expect_equal(unname(lc[["lci"]] / l[["lci"]]),
               unname(lc[["est"]] / l[["est"]]))
  expect_error(glassDelta(xg, yg, conf.level = 0.4, sides = "left"),
               "exceed 0.5")
})
