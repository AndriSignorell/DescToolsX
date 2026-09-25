test_that("lc returns an object of class 'Lc'", {
  expect_s3_class(lc(c(1,2,3,4,5)), "Lc")
})

test_that("lc object has required components", {
  obj <- lc(c(1,2,3,4,5))
  expect_named(obj, c("p","L","L.general","Gini","x","n"))
})

test_that("lc p starts at 0 and ends at 1", {
  obj <- lc(runif(50))
  expect_equal(obj$p[1], 0)
  expect_equal(tail(obj$p, 1), 1)
})

test_that("lc L starts at 0 and ends at 1", {
  obj <- lc(runif(50))
  expect_equal(obj$L[1], 0)
  expect_equal(tail(obj$L, 1), 1)
})

test_that("lc L is monotonically non-decreasing", {
  obj <- lc(runif(100))
  expect_true(all(diff(obj$L) >= -1e-10))
})

test_that("lc Gini is in [0, 1]", {
  obj <- lc(runif(100))
  expect_gte(obj$Gini, 0); expect_lte(obj$Gini, 1)
})

test_that("lc Gini = 0 for a constant vector (perfect equality)", {
  obj <- lc(rep(5, 20))
  expect_equal(obj$Gini, 0, tolerance = 1e-10)
})

test_that("lc formula interface returns class lc or LcList", {
  set.seed(1)
  x <- rlnorm(60)
  g <- sample(c("a","b"), 60, replace=TRUE)
  df <- data.frame(x=x, g=g)
  res <- lc(x ~ g, data=df)
  expect_true(inherits(res, "lc") || inherits(res, "LcList"))
})

test_that("predict.Lc returns a data frame with p and L columns", {
  obj <- lc(runif(50))
  pred <- predict(obj)
  expect_s3_class(pred, "data.frame")
  expect_named(pred, c("p","L"))
})

test_that("predict.Lc with conf.level adds lci and uci columns", {
  set.seed(5)
  obj <- lc(rlnorm(50))
  pred <- predict(obj, conf.level=0.95, R=200)
  expect_named(pred, c("p","L","lci","uci"))
})

test_that("lc na.rm = TRUE removes NAs", {
  x <- c(1, 2, NA, 4, 5)
  expect_s3_class(lc(x, na.rm=TRUE), "Lc")
})


# Additional branch coverage and reference checks
test_that("lc checks missing, negative and empty data", {
  expect_error(lc(numeric()), "empty input")
  expect_error(lc(c(NA_real_, NA_real_), na.rm = TRUE), "empty input")
  expect_error(lc(c(1, NA_real_)), "non-negative")
  expect_error(lc(c(-1, 2)), "non-negative")
})

test_that("lc weighted curves agree with hand-calculated population and income shares", {
  obj <- lc(c(4, 1, 2), n = c(1, 2, 1))
  expect_equal(obj$p, c(0, 0.5, 0.75, 1))
  expect_equal(obj$L, c(0, 0.25, 0.5, 1))
  expect_equal(obj$L.general, c(0, 0.5, 1, 2))
  clean <- lc(c(4, 1, 2, NA), n = c(1, 2, 1, 5), na.rm = TRUE)
  expect_equal(clean$L, obj$L)
  expect_equal(clean$p, obj$p)
})

test_that("lc formula dispatch handles one sample and a subset", {
  d <- data.frame(value = c(1, 2, 4, 8, 3, 6), group = rep(c("a", "b"), each = 3))
  expect_equal(lc(value ~ 1, data = d)$L, lc(d$value)$L)
  grouped <- lc(value ~ group, data = d, subset = value <= 6)
  expect_s3_class(grouped, "LcList")
  expect_equal(grouped$a$L, lc(c(1, 2, 4))$L)
  expect_equal(grouped$b$L, lc(c(3, 6))$L)
})

test_that("predict.Lc validates inputs and interpolates both curve types", {
  obj <- lc(c(1, 2, 4))
  expect_error(DescToolsX:::predict.Lc(list()), "class 'Lc'")
  expect_error(predict(obj, "0.5"), "numeric")
  expect_error(predict(obj, c(-0.1, 0.5)), "in \\[0, 1\\]")
  expect_error(predict(obj, 1.1), "in \\[0, 1\\]")
  expect_error(predict(obj, conf.level = 0), "conf.level")
  expect_error(predict(obj, conf.level = 1), "conf.level")
  expect_equal(predict(obj, general = TRUE), data.frame(p = obj$p, L = obj$L.general))
  p <- c(0, 0.5, 1)
  expect_equal(predict(obj, p)$L, c(0, 2 / 7, 1))
  expect_equal(predict(obj, p, general = TRUE)$L, c(0, 2 / 3, 7 / 3))
})

test_that("predict.Lc generalized intervals for constant data have exact endpoints", {
  # Correct degenerate case; the non-degenerate bootstrap regression is separate.
  obj <- lc(rep(4, 8))
  p <- c(0, 0.25, 0.5, 1)
  set.seed(814)
  ans <- predict(obj, p, general = TRUE, conf.level = 0.9, R = 199)
  expect_equal(ans$L, 4 * p)
  expect_equal(ans$lci, 4 * p)
  expect_equal(ans$uci, 4 * p)
})


# Regressions for fresh bootstrap samples and pointwise intervals.
test_that("Lorenz bootstrap varies across replicates and is reproducible", {
  obj <- lc(c(1, 2, 3, 5, 8, 13, 21, 34))
  set.seed(9001)
  a <- predict(obj, newdata = 0.5, conf.level = 0.95, R = 199)
  expect_gt(a$uci - a$lci, 0)
  set.seed(9001)
  b <- predict(obj, newdata = 0.5, conf.level = 0.95, R = 199)
  expect_identical(a, b)
})

test_that("Lorenz pointwise bootstrap agrees with a direct income-share reference", {
  x <- c(1, 3, 8)
  w <- c(2, 3, 1)
  obj <- lc(x, n = w)
  p <- c(0, 0.17, 0.43, 0.81, 1)
  n <- sum(w)
  # Income owned by the poorest p*n sample members, interpolating the
  # contribution of the next member. No lc() or predict.Lc() in the oracle.
  income_at <- function(z, p) {
    z <- sort(z)
    vapply(p, function(q) {
      rank <- q * length(z)
      k <- floor(rank)
      total <- sum(z[seq_len(k)])
      if (k < length(z)) total <- total + (rank - k) * z[k + 1L]
      total
    }, numeric(1))
  }
  for (general in c(FALSE, TRUE)) {
    set.seed(9002)
    actual <- predict(obj, p, conf.level = 0.9, general = general, R = 199)
    set.seed(9002)
    draws <- vapply(seq_len(199), function(i) {
      z <- x[sample.int(length(x), size = n, replace = TRUE, prob = w)]
      income_at(z, p) / if (general) n else sum(z)
    }, numeric(length(p)))
    expect_equal(actual$lci, unname(apply(draws, 1, quantile, probs = 0.05)))
    expect_equal(actual$uci, unname(apply(draws, 1, quantile, probs = 0.95)))
  }
})

test_that("lc retains the effective sample and prediction handles legacy missing values", {
  x <- c(1, NA, 3, 8, 13)
  w <- c(2, 9, 1, NA, 3)
  obj <- lc(x, n = w, na.rm = TRUE)
  ref <- lc(c(1, 3, 13), n = c(2, 1, 3))
  expect_identical(obj$x, ref$x)
  expect_identical(obj$n, ref$n)
  set.seed(9003)
  a <- predict(obj, 0.5, conf.level = 0.9, R = 199)
  set.seed(9003)
  expect_identical(predict(ref, 0.5, conf.level = 0.9, R = 199), a)
  # Older Lc objects retained the original unsorted vectors with NAs.
  legacy <- obj
  legacy$x <- x
  legacy$n <- w
  set.seed(9003)
  expect_identical(predict(legacy, 0.5, conf.level = 0.9, R = 199), a)
})

test_that("Lorenz bootstrap samples a single income value without sample's 1:x shortcut", {
  obj <- lc(5, n = 4)
  set.seed(9004)
  ans <- predict(obj, c(0, 0.5, 1), general = TRUE, conf.level = 0.9, R = 199)
  expect_equal(ans$lci, c(0, 2.5, 5))
  expect_equal(ans$uci, c(0, 2.5, 5))
})

test_that("Lorenz bootstrap handles zero-income resamples and missing prediction points", {
  obj <- lc(c(0, 1))
  set.seed(9005)
  ans <- predict(obj, c(0, 0.5, 1, NA_real_), conf.level = 0.9, R = 199)
  expect_true(all(is.finite(ans$lci[1:3])))
  expect_true(all(is.finite(ans$uci[1:3])))
  expect_equal(ans$lci[c(1, 3)], c(0, 1))
  expect_equal(ans$uci[c(1, 3)], c(0, 1))
  expect_true(is.na(ans$lci[4]))
  expect_true(is.na(ans$uci[4]))
  set.seed(9006)
  generalized <- predict(obj, 1, general = TRUE, conf.level = 0.9, R = 199)
  expect_equal(generalized$lci, 0)
  expect_equal(generalized$uci, 1)
})

test_that("predict.Lc validates scalar confidence levels before branching", {
  obj <- lc(c(1, 2, 3))
  for (bad in list(numeric(), c(0.9, 0.95), NaN, Inf, "0.95", TRUE))
    expect_error(predict(obj, conf.level = bad), "conf.level")
  expect_equal(predict(obj, conf.level = NA_real_), predict(obj))
})


test_that("predict.Lc uses percentile validation instead of the shared BCa default", {
  obj <- lc(c(1, 2, 3, 5, 8, 13))
  set.seed(9010)
  expect_silent(a <- predict(obj, 0.5, conf.level = 0.9, R = 199))
  set.seed(9010)
  expect_silent(b <- predict(obj, 0.5, conf.level = 0.9, R = 199, type = "perc"))
  expect_identical(a, b)
  expect_error(predict(obj, conf.level = 0.9, R = 199, type = "bca"),
               "supports only type")
  expect_error(predict(obj, conf.level = 0.9, R = 199, type = "norm"),
               "supports only type")
})
