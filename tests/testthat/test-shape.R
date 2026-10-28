# Tests for the shared skew()/kurt() machinery (shape.R).
# Additive to existing test-skew.R / test-kurt.R, does not replace them.

mom <- function(x, k) mean((x - mean(x))^k)

test_that("kurt: the three estimators for x = 1:4", {
  expect_equal(kurt(1:4, estimator = 1), -1.36)
  expect_equal(kurt(1:4, estimator = 2), -1.2)
  expect_equal(kurt(1:4, estimator = 3), -2.0775)
})

# reference values from the raw moments, independent of the C++ kernel
# (no e1071: it is not a declared dependency of the package)
refShape <- function(x, k, estimator) {
  n <- length(x)
  g <- mom(x, k) / mom(x, 2)^(k / 2) - if (k == 4) 3 else 0
  if (k == 3)
    switch(estimator, g, g * sqrt(n * (n - 1)) / (n - 2),
           g * ((n - 1) / n)^1.5)
  else
    switch(estimator, g, ((n + 1) * g + 6) * (n - 1) / ((n - 2) * (n - 3)),
           (g + 3) * ((n - 1) / n)^2 - 3)
}

test_that("skew/kurt agree with the raw-moment definitions", {
  set.seed(2)
  x <- rlnorm(37)
  for (e in 1:3) {
    expect_equal(kurt(x, estimator = e), refShape(x, 4, e))
    expect_equal(skew(x, estimator = e), refShape(x, 3, e))
  }
})

test_that("scale and shift invariance, also at extreme magnitudes", {
  set.seed(3)
  x <- rexp(30)
  for (s in c(1e-150, 1e-90, 1, 1e90, 1e150)) {
    expect_equal(kurt(x * s + 7 * s), kurt(x))
    expect_equal(skew(x * s + 7 * s), skew(x))
  }
  expect_equal(skew(-x), -skew(x))
})

test_that("classic interval: centred on est, half-width z * se", {
  set.seed(4)
  x <- rnorm(40)
  n <- 40
  z <- qnorm(0.975)
  se1 <- sqrt(24 * n * (n - 2) * (n - 3) / ((n + 1)^2 * (n + 3) * (n + 5)))
  slope <- c(1, (n - 1) * (n + 1) / ((n - 2) * (n - 3)), ((n - 1) / n)^2)
  for (e in 1:3) {
    r <- kurt(x, conf.level = 0.95, method = "classic", estimator = e)
    expect_named(r, c("est", "lci", "uci"))
    expect_equal(unname((r["lci"] + r["uci"]) / 2), unname(r["est"]))
    expect_equal(unname(r["uci"] - r["est"]), z * se1 * slope[e])
  }
  # Cramer's SEK for estimator 2
  sek <- sqrt(24 * n * (n - 1)^2 / ((n - 3) * (n - 2) * (n + 3) * (n + 5)))
  r <- kurt(x, conf.level = 0.95, method = "classic", estimator = 2)
  expect_equal(unname(r["uci"] - r["est"]), z * sek)
})

test_that("frequency weights equal the expanded sample", {
  x <- c(1, 2, 3, 10, 4)
  w <- c(3, 1, 2, 1, 4)
  for (e in 1:3) {
    expect_equal(kurt(x, weights = w, estimator = e),
                 kurt(rep(x, w), estimator = e))
    expect_equal(skew(x, weights = w, estimator = e),
                 skew(rep(x, w), estimator = e))
    expect_equal(kurt(x, weights = w, conf.level = 0.9, method = "classic",
                      estimator = e),
                 kurt(rep(x, w), conf.level = 0.9, method = "classic",
                      estimator = e))
  }
  # zero weight = absent observation
  expect_equal(kurt(c(x, 99), weights = c(w, 0)), kurt(x, weights = w))
})

test_that("weighted bootstrap resamples the expanded sample", {
  x <- c(1, 2, 3, 10, 4)
  w <- c(3, 1, 2, 1, 4)
  set.seed(5); a <- kurt(x, weights = w, conf.level = 0.95, R = 199,
                         type = "perc")
  set.seed(5); b <- kurt(rep(x, w), conf.level = 0.95, R = 199,
                         type = "perc")
  expect_equal(a, b)
  expect_error(kurt(x, weights = w + 0.5, conf.level = 0.95),
               "integer frequency weights")
  expect_silent(kurt(x, weights = w + 0.5, conf.level = 0.95,
                     method = "classic"))
})

test_that("matrix input is flattened identically in all paths", {
  set.seed(6)
  m <- matrix(rexp(24), nrow = 4)
  expect_equal(kurt(m), kurt(as.vector(m)))
  set.seed(7); a <- skew(m, conf.level = 0.95, R = 199, type = "perc")
  set.seed(7); b <- skew(as.vector(m), conf.level = 0.95, R = 199,
                         type = "perc")
  expect_equal(a, b)
  m[2, 3] <- NA
  expect_equal(kurt(m, na.rm = TRUE), kurt(as.vector(m), na.rm = TRUE))
})

test_that("NA in x or weights: pairwise removal, same in all paths", {
  x <- c(1, 2, NA, 10, 4, 5, 7)
  w <- c(3, NA, 2, 1, 4, 1, 1)
  ok <- !is.na(x) & !is.na(w)
  expect_equal(kurt(x, weights = w, na.rm = TRUE),
               kurt(x[ok], weights = w[ok]))
  expect_true(is.na(kurt(x, weights = w)))
  expect_identical(kurt(x, conf.level = 0.95),
                   c(est = NA_real_, lci = NA_real_, uci = NA_real_))
  set.seed(8); a <- kurt(x, weights = w, na.rm = TRUE, conf.level = 0.95,
                         R = 199, type = "perc")
  set.seed(8); b <- kurt(rep(x[ok], w[ok]), conf.level = 0.95, R = 199,
                         type = "perc")
  expect_equal(a, b)
})

test_that("undefined cases: NA with a warning, never NaN", {
  expect_warning(r <- kurt(numeric(0)), "needs n >= 2")
  expect_identical(r, NA_real_)
  expect_warning(r <- kurt(c(NA, NA), na.rm = TRUE), "needs n >= 2")
  expect_identical(kurt(c(NA, NA)), NA_real_)
  expect_warning(r <- kurt(rep(3, 10)), "constant")
  expect_identical(r, NA_real_)
  expect_warning(r <- kurt(rep(3, 10), conf.level = 0.95), "constant")
  expect_identical(r, c(est = NA_real_, lci = NA_real_, uci = NA_real_))
  expect_warning(r <- kurt(1:3, estimator = 2), "needs n >= 4")
  expect_identical(r, NA_real_)
  expect_warning(skew(1:2, estimator = 2), "needs n >= 3")
  expect_silent(kurt(1:3, estimator = 3))
  # estimate defined, SE not: estimate kept, interval NA
  expect_warning(r <- kurt(1:3, conf.level = 0.95, method = "classic"),
                 "standard error undefined")
  expect_false(is.na(r[["est"]]))
  expect_true(all(is.na(r[c("lci", "uci")])))
  # single observation with frequency 5 is constant
  expect_warning(kurt(3, weights = 5), "constant")
})

test_that("bootstrap: every offered type gives est, lci, uci", {
  set.seed(9)
  x <- rlnorm(40)
  for (tp in c("bca", "perc", "basic", "norm")) {
    r <- suppressWarnings(kurt(x, conf.level = 0.9, R = 499, type = tp))
    expect_named(r, c("est", "lci", "uci"))
    expect_equal(r[["est"]], kurt(x))
    expect_true(r[["lci"]] < r[["uci"]])
  }
  expect_error(kurt(x, conf.level = 0.9, type = "stud"), "not supported")
  expect_error(kurt(x, conf.level = 0.9, strata = rep(1, 40)),
               "unsupported argument")
})

test_that("degenerate bootstrap: NA bounds with a warning", {
  x <- c(rep(0, 3), 1)
  set.seed(10)
  expect_warning(r <- kurt(x, conf.level = 0.95, R = 199, type = "perc"),
                 "undefined")
  expect_false(is.na(r[["est"]]))
  expect_true(all(is.na(r[c("lci", "uci")])))
})

test_that("sides", {
  set.seed(11)
  x <- rnorm(30)
  two <- kurt(x, conf.level = 0.9, method = "classic")
  left <- kurt(x, conf.level = 0.95, method = "classic", sides = "left")
  right <- kurt(x, conf.level = 0.95, method = "classic", sides = "right")
  expect_equal(left[["lci"]], two[["lci"]])
  expect_identical(left[["uci"]], Inf)
  expect_equal(right[["uci"]], two[["uci"]])
  expect_identical(right[["lci"]], -Inf)
  expect_error(kurt(x, conf.level = 0.4, sides = "left"), "> 0.5")
})

test_that("argument checks", {
  expect_error(kurt("a"), "'x' must be numeric")
  expect_error(kurt(1:5, estimator = 4), "'estimator'")
  expect_error(kurt(1:5, estimator = c(1, 2)), "'estimator'")
  expect_error(kurt(1:5, na.rm = NA), "'na.rm'")
  expect_error(kurt(1:5, conf.level = c(0.9, 0.95)), "'conf.level'")
  expect_error(kurt(1:5, conf.level = 1), "'conf.level'")
  expect_error(kurt(1:5, conf.level = NULL), "'conf.level'")
  expect_error(kurt(1:5, weights = 1:4), "same length")
  expect_error(kurt(1:5, weights = c(1, 1, -1, 1, 1)), "non-negative")
  expect_error(kurt(1:5, weights = rep(0, 5)), "positive sum")
  expect_error(kurt(c(1:4, Inf)), "infinite")
})

test_that("point estimate is an unnamed scalar", {
  expect_null(names(kurt(1:10)))
  expect_null(names(skew(1:10)))
})

test_that("estimator must be numeric (positional switch in the kernels)", {
  expect_error(kurt(1:10, estimator = "2"), "'estimator'")
  expect_error(skew(1:10, estimator = NA), "'estimator'")
  expect_equal(kurt(1:10, estimator = 2L), kurt(1:10, estimator = 2))
})

test_that("no overflow in the centring for any finite input", {
  x <- c(-1e308, 1.7e308, 1.7e308)
  expect_equal(kurt(x, estimator = 1), -1.5)
  expect_equal(skew(x, estimator = 1), -1 / sqrt(2))
  expect_equal(kurt(-x, estimator = 1), -1.5)
})

test_that("shift invariance with a large common offset (exact data)", {
  # all values exactly representable; the offset must not change anything
  x <- c(0, 2, 4, 6)
  for (e in 1:3) {
    expect_equal(skew(1e16 + x, estimator = e), skew(x, estimator = e),
                 tolerance = 1e-12)
    expect_equal(kurt(1e16 + x, estimator = e), kurt(x, estimator = e),
                 tolerance = 1e-12)
  }
  expect_equal(skew(1e16 + x), 0)
  # weighted, asymmetric, other offsets and signs
  x <- c(0, 2, 4, 6, 20)
  w <- c(3, 1, 2, 1, 1)
  for (off in c(2^40, 1e16, -1e16, 2^56)) {  # 16 * x: exact at ulp(2^56) = 16
    expect_equal(skew(off + 16 * x, weights = w), skew(x, weights = w),
                 tolerance = 1e-12)
    expect_equal(kurt(off + 16 * x, weights = w), kurt(x, weights = w),
                 tolerance = 1e-12)
  }
  # the direct C++ entry points as well
  expect_equal(skew_cpp(1e16 + c(0, 2, 4, 6)), 0)
  expect_equal(kurt_cpp(1e16 + c(0, 2, 4, 6)), -1.36)
})

test_that("zero weight means absent, also for NA/Inf x and in C++", {
  expect_equal(kurt(c(1, 2, 3, 7, NA), weights = c(1, 1, 1, 2, 0)),
               kurt(c(1, 2, 3, 7), weights = c(1, 1, 1, 2)))
  expect_equal(skew(c(1, 2, 3, 7, Inf), weights = c(1, 1, 1, 2, 0)),
               skew(c(1, 2, 3, 7), weights = c(1, 1, 1, 2)))
  # a MISSING weight keeps the observation missing
  expect_true(is.na(kurt(c(1, 2, 3, 7), weights = c(1, 1, NA, 1))))
  # direct C++ entry points skip zero weights in the scale factors too
  expect_equal(kurt_weighted_cpp(c(1, 2, 3, 7, 1e300), c(1, 1, 1, 2, 0)),
               kurt_weighted_cpp(c(1, 2, 3, 7), c(1, 1, 1, 2)))
  expect_equal(skew_weighted_cpp(c(1, 2, 3, 7, -1e300), c(1, 1, 1, 2, 0)),
               skew_cpp(c(1, 2, 3, 7, 7)))
})

test_that("huge weight sums: finite results or a clear error", {
  x <- c(1, 2, 3, 10, 4)
  expect_error(kurt(x, weights = c(1e308, 1e308, 1, 1, 1)), "not finite")
  r <- kurt(x, weights = rep(1e300, 5), conf.level = 0.95, method = "classic")
  expect_true(all(is.finite(r)))
  # weights scaled by a constant give the same g_2 (estimator 1 has no
  # n-correction)
  expect_equal(kurt(x, weights = rep(1e300, 5), estimator = 1),
               kurt(x, estimator = 1))
})
