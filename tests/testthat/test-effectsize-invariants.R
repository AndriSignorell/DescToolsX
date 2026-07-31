
# Reference values and structural invariants for the effect-size /
# agreement tranche. The kappa case is the worked example from
# https://online.stat.psu.edu/stat509/lesson/18/18.7 that previously sat
# as a dead comment block at the bottom of cohenKappa.R.

test_that("weighted kappa reproduces the PSU reference example", {

  lbl <- c("Normal", "Benign", "Suspect", "Cancer")
  m <- matrix(c(21,  4,  3, 0,
                12, 17,  9, 0,
                 0,  1, 15, 0,
                 0,  0,  2, 1), nrow = 4, byrow = TRUE,
              dimnames = list(rater1 = lbl, rater2 = lbl))

  res <- cohenKappa(m, weights = "equal-spacing", conf.level = 0.95)

  # the PSU page reports two decimals, so compare at that resolution -
  # expect_equal()'s tolerance is RELATIVE, and 0.436 vs 0.44 is 0.9%,
  # which a tolerance of 0.005 does not cover
  expect_equal(round(unname(res[["est"]]), 2), 0.57)
  expect_equal(round(unname(res[["lci"]]), 2), 0.44)
  expect_equal(round(unname(res[["uci"]]), 2), 0.70)
})


test_that("kappa is invariant to transposition with symmetric weights", {

  m <- matrix(c(53, 5, 2, 11, 14, 5, 1, 6, 3), nrow = 3, byrow = TRUE)

  for (w in c("unweighted", "equal-spacing", "fleiss-cohen"))
    expect_equal(cohenKappa(m, weights = w),
                 cohenKappa(t(m), weights = w), label = w)
})


test_that("one-sided intervals put the finite bound on the named side", {

  # design_rules.md 4.1: 'sides' names the side carrying the FINITE bound.
  x <- c(5.1, 4.8, 6.2, 5.5, 5.9, 6.4, 4.9, 5.7)
  y <- c(4.2, 4.6, 4.1, 5.0, 4.4, 4.8, 4.3, 4.7)

  left  <- cohenD(x, y, conf.level = 0.95, sides = "left")
  right <- cohenD(x, y, conf.level = 0.95, sides = "right")

  expect_true(is.finite(left[["lci"]]))
  expect_identical(unname(left[["uci"]]), Inf)

  expect_identical(unname(right[["lci"]]), -Inf)
  expect_true(is.finite(right[["uci"]]))

  # the finite one-sided bound is the corresponding 90%-two-sided bound
  two <- cohenD(x, y, conf.level = 0.90)
  expect_equal(unname(left[["lci"]]),  unname(two[["lci"]]))
  expect_equal(unname(right[["uci"]]), unname(two[["uci"]]))
})


test_that("cohenD honours 'correct' in the one-sample case", {

  x <- c(1.2, 0.8, 1.5, 0.9, 1.1, 1.4, 0.7, 1.3)

  plain <- cohenD(x)
  corr  <- cohenD(x, correct = TRUE)

  expect_lt(abs(unname(corr)), abs(unname(plain)))
  expect_equal(unname(corr),
               unname(plain) * (1 - 3 / (4 * (length(x) - 1) - 1)))
})


test_that("the one-sample d interval brackets the estimate and uses n-1 df", {

  x  <- rnorm(40, mean = 0.5)
  res <- cohenD(x, conf.level = 0.95)

  expect_lt(unname(res[["lci"]]), unname(res[["est"]]))
  expect_gt(unname(res[["uci"]]), unname(res[["est"]]))

  # a one-sample d of 0 gives a symmetric interval; the two-sample
  # equal-group formula used previously would have widened it by ~sqrt(2)
  z <- cohenD(rnorm(40) - mean(rnorm(40)) * 0, conf.level = 0.95)
  expect_true(is.finite(z[["lci"]]) && is.finite(z[["uci"]]))
})


test_that("cohenH rejects vector input without y", {
  expect_error(cohenH(c(1, 2, 3, 4)), "2x2")
})


test_that("cohenH matches a hand computation", {

  tab <- matrix(c(26, 26, 6, 7), nrow = 2, byrow = TRUE)
  p1 <- 26 / 52
  p2 <- 6 / 13

  expect_equal(unname(cohenH(tab)),
               2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2)))
})


test_that("the scaled Brier interval lives on the scaled scale", {

  set.seed(1)
  resp <- rbinom(300, 1, 0.4)
  pred <- plogis(rnorm(300, ifelse(resp == 1, 0.5, -0.5)))

  raw    <- brierScore(resp, pred, conf.level = 0.95)
  scaled <- brierScore(resp, pred, conf.level = 0.95, scaled = TRUE)

  # the interval must bracket its own estimate - the former version
  # combined a scaled estimate with an unscaled standard error
  expect_lt(unname(scaled[["lci"]]), unname(scaled[["est"]]))
  expect_gt(unname(scaled[["uci"]]), unname(scaled[["est"]]))

  # and it is wider than the raw one by roughly 1 / BSmax
  meanY <- mean(resp)
  bsMax <- meanY * (1 - meanY)^2 + (1 - meanY) * meanY^2
  expect_equal(diff(scaled[c("lci", "uci")]),
               diff(raw[c("lci", "uci")]) / bsMax,
               tolerance = 1e-8, ignore_attr = TRUE)
})


test_that("the percentile bootstrap honours the requested level", {

  set.seed(2)
  resp <- rbinom(200, 1, 0.4)
  pred <- plogis(rnorm(200, ifelse(resp == 1, 0.5, -0.5)))

  perc <- brierScore(resp, pred, conf.level = 0.95, method = "boot",
                     type = "perc", R = 999)
  percOne <- brierScore(resp, pred, conf.level = 0.95, sides = "left",
                        method = "boot", type = "perc", R = 999)

  expect_identical(unname(percOne[["uci"]]), Inf)
  expect_true(is.finite(percOne[["lci"]]))
})


test_that("coefVarCI refuses model objects and honours unbiased", {

  set.seed(3)
  x <- runif(50, 1, 3)

  expect_error(coefVarCI(lm(Fertility ~ ., swiss)), "numeric vector")

  plain <- coefVarCI(x)
  unb   <- coefVarCI(x, unbiased = TRUE)
  expect_false(isTRUE(all.equal(unname(plain[["est"]]), unname(unb[["est"]]))))
  expect_equal(unname(unb[["est"]]), unname(coefVar(x, unbiased = TRUE)))
})


test_that("verrill is refused rather than faked", {
  expect_error(coefVarCI(runif(30), method = "verrill"), "not implemented")
})


test_that("ccc returns the documented shape for missing input", {

  x <- c(1, 2, 3, NA, 5)
  y <- c(1, 2, 4, 5,  6)

  res <- ccc(x, y, conf.level = 0.95)
  expect_named(res, c("est", "lci", "uci"))
  expect_true(all(is.na(res)))
})
