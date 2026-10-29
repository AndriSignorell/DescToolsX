
# Score reference values solve the Koopman score equation independently of
# the implementation: by direct inversion with 40-digit arithmetic (mpmath)
# for the large-table cases, and by a separate double-precision solver for
# the others. Closed-form binomial reductions and reciprocal invariance
# provide additional checks.

test_that("relRisk() returns the point estimate only when conf.level is NA", {
  m <- matrix(c(78, 50, 1422, 950), nrow = 2)
  expect_equal(relRisk(m), (78 / 1500) / (50 / 1000))
  expect_null(names(relRisk(m)))
})


test_that("relRisk() score interval matches the score equation", {

  m <- matrix(c(78, 50, 1422, 950), nrow = 2)
  r <- relRisk(m, conf.level = 0.95)
  expect_named(r, c("est", "lci", "uci"))
  expect_equal(unname(r), c(1.04, 0.7375966894, 1.4682950512), tolerance = 1e-7)

  mm <- cbind(c(9, 20), c(41, 29))
  r <- relRisk(t(mm), conf.level = 0.95)
  expect_equal(unname(r), c(0.5298570227, 0.2869143427, 0.8869266581),
               tolerance = 1e-7)

  r <- relRisk(matrix(c(20, 2, 5, 38), nrow = 2), conf.level = 0.95)
  expect_equal(unname(r), c(16, 4.7485701889, 58.5110162120), tolerance = 1e-7)
})


test_that("relRisk() handles a second row without non-events (x2 == n2)", {

  # rows: exposed 2 of 5, unexposed 5 of 5 -> RR = 0.4
  # regression: the former closed-form cubic returned (0.700, 0.938) here,
  # i.e. a lower bound ABOVE the estimate
  m <- matrix(c(2, 5, 3, 0), nrow = 2)
  r <- relRisk(m, conf.level = 0.95)
  expect_equal(unname(r), c(0.4, 0.1176207742, 0.9378042349), tolerance = 1e-6)

  r <- relRisk(matrix(c(4, 10, 6, 0), nrow = 2), conf.level = 0.95)
  expect_equal(unname(r), c(0.4, 0.1681803297, 0.6873262303), tolerance = 1e-6)

  r <- relRisk(matrix(c(9, 5, 1, 0), nrow = 2), conf.level = 0.95)
  expect_equal(unname(r), c(0.9, 0.5958499732, 1.6170723132), tolerance = 1e-6)
})


test_that("relRisk() score interval always brackets the estimate", {
  
  cases <- do.call(rbind, lapply(c(5L, 10L, 12L), function(n1)
    do.call(rbind, lapply(c(5L, 10L, 12L), function(n2)
      expand.grid(x1 = 0:n1, x2 = 0:n2, n1 = n1, n2 = n2)))))
  
  lab <- sprintf("x1=%d x2=%d n1=%d n2=%d",
                 cases$x1, cases$x2, cases$n1, cases$n2)
  
  r <- Map(function(x1, x2, n1, n2)
    relRisk(matrix(c(x1, x2, n1 - x1, n2 - x2), nrow = 2), conf.level = 0.95),
    cases$x1, cases$x2, cases$n1, cases$n2)
  
  est <- vapply(r, `[[`, 0, "est")
  lci <- vapply(r, `[[`, 0, "lci")
  uci <- vapply(r, `[[`, 0, "uci")
  
  # No events in either group leave the estimate undefined (0/0).
  defined <- !(cases$x1 == 0 & cases$x2 == 0)
  
  # One assertion per property reports all offending tables on failure.
  expect_equal(lab[defined & is.na(est)],          character(0))
  expect_equal(lab[is.na(lci) | is.na(uci)],       character(0))
  expect_equal(lab[!(lci <= uci + 1e-12)],         character(0))
  expect_equal(lab[is.finite(est) &
                     !(lci <= est + 1e-8 & est <= uci + 1e-8)], character(0))
})


test_that("relRisk() handles a first row without non-events (x1 == n1)", {

  # regression: x1 == n1 puts the constrained MLE on p1 = 1. The former
  # closed-form cubic returned a far too narrow interval here, or NaN.
  # Reference values solve the score equation numerically.
  r <- relRisk(matrix(c(5, 11, 0, 1), nrow = 2), conf.level = 0.95)
  expect_equal(r[["est"]], 1.0909090909, tolerance = 1e-8)
  expect_equal(r[["lci"]], 0.6098435589, tolerance = 1e-6)
  expect_equal(r[["uci"]], 1.5476999048, tolerance = 1e-6)

  # ... the old bound was 1.0625, an interval that excluded 1 altogether
  expect_lt(r[["lci"]], 1)

  r <- relRisk(matrix(c(4, 1, 0, 3), nrow = 2), conf.level = 0.95)
  expect_equal(r[["lci"]], 1.1761551839, tolerance = 1e-6)
  expect_equal(r[["uci"]], 21.9359527692, tolerance = 1e-6)

  # both rows on the boundary keeps its closed form
  r <- relRisk(matrix(c(4, 4, 0, 0), nrow = 2), conf.level = 0.95)
  expect_equal(r[["lci"]], 4 / (4 + qnorm(0.975)^2))
  expect_equal(r[["uci"]], (4 + qnorm(0.975)^2) / 4)

})


test_that("relRisk() score interval keeps the documented boundary cases", {

  z <- qnorm(0.975)

  # both groups all events
  r <- relRisk(matrix(c(10, 10, 0, 0), nrow = 2), conf.level = 0.95)
  expect_equal(unname(r[c("lci", "uci")]),
               c(10 / (10 + z^2), (10 + z^2) / 10), tolerance = 1e-8)

  # no events at all
  r <- relRisk(matrix(c(0, 0, 7, 9), nrow = 2), conf.level = 0.95)
  expect_equal(unname(r[c("lci", "uci")]), c(0, Inf))

  # unexposed group without events -> upper bound is infinite
  r <- relRisk(matrix(c(4, 0, 6, 10), nrow = 2), conf.level = 0.95)
  expect_true(is.finite(r[["lci"]]) && r[["lci"]] > 0)
  expect_identical(r[["uci"]], Inf)

  # exposed group without events -> lower bound is zero
  r <- relRisk(matrix(c(0, 4, 10, 6), nrow = 2), conf.level = 0.95)
  expect_identical(r[["lci"]], 0)
  expect_true(is.finite(r[["uci"]]) && r[["uci"]] > 0)
})


test_that("relRisk() wald and use-or return ordered named bounds", {

  mm <- t(cbind(c(9, 20), c(41, 29)))

  w <- relRisk(mm, conf.level = 0.95, method = "wald")
  expect_named(w, c("est", "lci", "uci"))
  expect_equal(unname(w[c("lci", "uci")]),
               exp(log((9 / 29) / (41 / 70)) + c(-1, 1) *
                     qnorm(0.975) * sqrt(1 / 9 - 1 / 29 + 1 / 41 - 1 / 70)), tolerance = 1e-7)

  o <- relRisk(mm, conf.level = 0.95, method = "use-or")
  expect_named(o, c("est", "lci", "uci"))
  expect_true(o[["lci"]] < o[["est"]] && o[["est"]] < o[["uci"]])

  # No log-Wald interval exists with zero observed event counts.
  expect_warning(
    d <- relRisk(matrix(c(0, 4, 10, 6), nrow = 2), conf.level = 0.95,
                 method = "wald"), "positive event counts")
  expect_equal(unname(d), c(0, NA_real_, NA_real_))
})


test_that("every flavour of NA is accepted as conf.level", {

  # regression: the check tested !is.numeric() before admitting NA, and NA is
  # logical - so relRisk(m) rejected the function's own default
  m <- matrix(c(78, 50, 1422, 950), nrow = 2)
  target <- (78 / 1500) / (50 / 1000)

  expect_equal(relRisk(m), target)
  expect_equal(relRisk(m, conf.level = NA), target)
  expect_equal(relRisk(m, conf.level = NA_real_), target)
  expect_equal(relRisk(m, conf.level = NA_integer_), target)

  expect_error(relRisk(m, conf.level = NaN), "conf.level")
  expect_error(relRisk(m, conf.level = TRUE), "conf.level")
  expect_error(relRisk(m, conf.level = "0.95"), "conf.level")
  expect_error(relRisk(m, conf.level = c(NA, 0.95)), "conf.level")

})


test_that("relRisk() validates its arguments", {

  m <- matrix(c(78, 50, 1422, 950), nrow = 2)

  expect_error(relRisk(matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)), "2x2")
  expect_error(relRisk(matrix(c(1, -2, 3, 4), nrow = 2)), "non-negative")
  expect_error(relRisk(matrix(c(1.5, 2, 3, 4), nrow = 2)), "integer")
  expect_error(relRisk(matrix(c(0, 2, 0, 4), nrow = 2)), "positive totals")
  expect_error(relRisk(matrix(c(1, 2, NA, 4), nrow = 2)), "missing")
  expect_error(relRisk(m, conf.level = 1), "conf.level")
  expect_error(relRisk(m, conf.level = 0), "conf.level")
  expect_error(relRisk(m, conf.level = 0.95, delta = -1), "delta")
})



test_that("integer tables and double matrices give identical score intervals", {
  m <- matrix(rep(1000L, 4), nrow = 2)
  d <- m
  storage.mode(d) <- "double"
  expect_warning(r <- relRisk(m, conf.level = 0.95), NA)
  expect_equal(r, relRisk(d, conf.level = 0.95))
  expect_true(all(is.finite(r)))
  expect_lt(r[["lci"]], 1)
  expect_gt(r[["uci"]], 1)
  expect_equal(relRisk(as.table(m), conf.level = 0.95), r)
})


test_that("Wald uses a consistent four-cell correction", {
  m <- matrix(c(0, 4, 10, 6), nrow = 2)
  corrected <- m + 0.5
  n <- rowSums(corrected)
  expected <- (corrected[1, 1] / n[1]) / (corrected[2, 1] / n[2])
  se <- sqrt(sum(1 / corrected[, 1] - 1 / n))
  r <- relRisk(m, conf.level = 0.95, method = "wald", delta = 0.5)
  expect_equal(unname(r), c(expected,
    exp(log(expected) + c(-1, 1) * qnorm(0.975) * se)))
  expect_equal(relRisk(m, method = "wald", delta = 0.5), expected)
  expect_equal(relRisk(m, conf.level = 0.95, delta = 0.5),
               relRisk(m, conf.level = 0.95))

  all_events <- matrix(c(10, 10, 0, 0), nrow = 2)
  expect_warning(r <- relRisk(all_events, conf.level = 0.95,
                              method = "wald"), "degenerate")
  expect_equal(unname(r), c(1, 1, 1))
  r <- relRisk(all_events, conf.level = 0.95, method = "wald", delta = 0.5)
  expect_true(all(is.finite(r)))
  expect_lt(r[["lci"]], 1)
  expect_gt(r[["uci"]], 1)

  for (counts in list(c(4, 0, 6, 10), c(0, 0, 10, 10))) {
    expect_warning(r <- relRisk(matrix(counts, 2), conf.level = 0.95,
                                method = "wald"), "positive event counts")
    expect_true(all(is.na(r[c("lci", "uci")])))
  }
})


test_that("non-finite counts and corrections are rejected explicitly", {
  m <- matrix(c(1, 2, 3, 4), 2)
  for (v in c(Inf, -Inf)) {
    bad <- m
    bad[1, 1] <- v
    expect_error(relRisk(bad), "finite counts")
    expect_error(relRisk(m, delta = v), "delta")
  }
  for (v in list(NA_real_, NaN, numeric(0), c(0, 1), "0", TRUE))
    expect_error(relRisk(m, delta = v), "delta")
})


test_that("vector input follows explicitly supplied factor levels", {
  exposure <- c(rep(1, 10), rep(0, 10))
  outcome <- c(rep(1, 4), rep(0, 6), rep(1, 2), rep(0, 8))
  m <- matrix(c(4, 2, 6, 8), 2)
  r <- relRisk(factor(exposure, levels = c(1, 0)),
               factor(outcome, levels = c(1, 0)), conf.level = 0.95)
  expect_equal(r, relRisk(m, conf.level = 0.95))
  expect_equal(relRisk(exposure, outcome), relRisk(table(exposure, outcome)))
})


test_that("score and Wald intervals are reciprocal under row reversal", {
  cases <- list(c(5, 0, 0, 5), c(2, 5, 3, 0), c(5, 11, 0, 1), c(4, 0, 6, 10),
                c(0, 4, 10, 6), c(4, 7, 0, 0), c(78, 50, 1422, 950))
  for (counts in cases) {
    m <- matrix(counts, 2)
    for (method in c("score", "wald")) {
      args <- list(conf.level = 0.95, method = method,
                   delta = if (method == "wald") 0.5 else 0)
      r <- do.call(relRisk, c(list(x = m), args))
      rev <- do.call(relRisk, c(list(x = m[2:1, ]), args))
      expect_equal(unname(rev), 1 / unname(r[c("est", "uci", "lci")]),
                   tolerance = 1e-6)
    }
  }
})


test_that("one-sided intervals use the corresponding adjusted two-sided bound", {
  m <- matrix(c(78, 50, 1422, 950), 2)
  for (method in c("score", "wald", "use-or")) {
    central <- relRisk(m, conf.level = 0.90, method = method)
    left <- relRisk(m, conf.level = 0.95, sides = "left", method = method)
    right <- relRisk(m, conf.level = 0.95, sides = "right", method = method)
    expect_named(left, c("est", "lci", "uci"))
    expect_named(right, c("est", "lci", "uci"))
    expect_equal(left[["est"]], central[["est"]])
    expect_equal(right[["est"]], central[["est"]])
    expect_equal(left[["lci"]], central[["lci"]])
    expect_identical(left[["uci"]], Inf)
    expect_identical(right[["lci"]], 0)
    expect_equal(right[["uci"]], central[["uci"]])
  }
  for (side in c("left", "right"))
    expect_error(relRisk(m, conf.level = 0.5, sides = side), "above 0.5")
})


test_that("use-or reports unavailable intervals at reference-risk boundaries", {
  for (counts in list(c(4, 0, 6, 10), c(4, 10, 6, 0), c(0, 0, 10, 10))) {
    expect_warning(r <- relRisk(matrix(counts, 2), conf.level = 0.95,
                                method = "use-or"), "0 < x2 / n2 < 1")
    expect_true(all(is.na(r[c("lci", "uci")])))
  }
})


test_that("use-or transforms infinite OR limits without Inf divided by Inf", {
  # Isolate transformation from the separate oddsRatio() implementation.
  helper <- .relRiskUseOr
  env <- new.env(parent = environment(helper))
  env$oddsRatio <- function(...) c(uci = Inf, est = 1, lci = 0)
  environment(helper) <- env
  m <- matrix(c(4, 2, 6, 8), 2)
  expect_equal(helper(m, x2 = 2, n2 = 10, conf.level = 0.95),
               c(lci = 0, uci = 5))
  env$oddsRatio <- function(...) c(est = 1, lci = 0.5)
  expect_error(helper(m, 2, 10, 0.95), "expected")
})



test_that("all events versus no events agrees with an independent reference", {
  m <- matrix(c(5, 0, 0, 5), 2)
  r <- relRisk(m, conf.level = 0.95)
  expect_identical(r[["est"]], Inf)
  expect_identical(r[["uci"]], Inf)
  expect_equal(r[["lci"]], 2.3015888581, tolerance = 1e-9)
  mirror <- relRisk(m[2:1, ], conf.level = 0.95)
  expect_identical(mirror[["lci"]], 0)
  expect_equal(mirror[["uci"]] * r[["lci"]], 1, tolerance = 1e-9)
})


test_that("small score bounds retain relative precision", {
  # Here the constrained reference probability equals 1 at the lower bound.
  # The lower bound is therefore the Wilson lower bound for x1/n1.
  z2 <- qnorm(0.975)^2
  for (n in c(1000, 5000, 1e13)) {
    m <- matrix(c(1, 3, n - 1, 0), 2)
    # Rationalized Wilson formula: avoids subtracting nearly equal terms.
    reference <- 2 / (n * (2 + z2 + sqrt(z2^2 + 4 * z2 * (1 - 1 / n))))
    r <- relRisk(m, conf.level = 0.95)
    expect_equal(r[["lci"]] / reference, 1, tolerance = 1e-8)
    mirror <- relRisk(m[2:1, ], conf.level = 0.95)
    expect_equal(mirror[["uci"]] * r[["lci"]], 1, tolerance = 1e-8)
  }
})


test_that("score boundary terms distinguish compatible and impossible data", {
  expect_identical(.relRiskScoreTerm(0, 5, 0), 0)
  expect_identical(.relRiskScoreTerm(5, 5, 1), 0)
  expect_identical(.relRiskScoreTerm(1, 5, 0), Inf)
  expect_identical(.relRiskScoreTerm(4, 5, 1), Inf)
  expect_equal(.relRiskScoreTerm(2, 5, 0.5), 0.2)
})


test_that("numerical bracketing failure cannot silently become an open bound", {
  helper <- .relRiskScoreNumeric
  env <- new.env(parent = environment(helper))
  env$.relRiskScoreStat <- function(...) 0
  environment(helper) <- env
  expect_error(helper(1, 3, 1000, 3, qnorm(0.975)), "Could not bracket")
})


test_that("vector errors explain y and overflowing totals are rejected", {
  expect_error(relRisk(1:4), "supply 'y'")
  expect_error(relRisk(matrix(1e308, 2, 2)), "finite totals")
  expect_error(relRisk(matrix(1 + 1i, 2, 2)), "numeric")
  expect_error(relRisk(matrix(1, 2, 2), delta = 1i), "delta")
})



test_that("score intervals remain accurate for large and imbalanced tables", {
  cases <- list(
    c(1, 999, 1000, 1000),
    c(1, 99999, 1e5, 1e5),
    c(50, 40, 1e7, 1e7),
    c(9990, 9995, 1e4, 1e4)
  )
  references <- list(
    c(0.0001767229699920041, 0.005648210886368910),
    c(0.000001765265382908588, 0.00005664766307100651),
    c(0.8272165531272541, 1.888864600273311),
    c(0.9986025814590217, 1.000294178076961)
  )
  for (i in seq_along(cases)) {
    counts <- cases[[i]]
    m <- matrix(c(counts[1:2], counts[3:4] - counts[1:2]), 2)
    expect_warning(r <- relRisk(m, conf.level = 0.95), NA)
    expect_true(all(is.finite(r)))
    expect_equal(unname(r[c("lci", "uci")]) / references[[i]],
                 c(1, 1), tolerance = 1e-8)
    expect_lt(r[["lci"]], r[["est"]])
    expect_gt(r[["uci"]], r[["est"]])
    mirror <- relRisk(m[2:1, ], conf.level = 0.95)
    expect_equal(unname(mirror) * unname(r[c("est", "uci", "lci")]),
                 rep(1, 3), tolerance = 1e-8)
  }

  m <- matrix(c(50, 40, 1e7 - 50, 1e7 - 40), 2)
  r <- relRisk(m, conf.level = 0.95)
  expect_equal(unname(r[c("lci", "uci")]),
               c(0.82721655313, 1.88886460027), tolerance = 1e-9)
})


test_that("use-or needs positive cells or an explicit correction", {

  # the odds-ratio interval it transforms is the Wald interval, which is
  # unavailable for a zero cell unless delta > 0 (no silent correction)
  m <- matrix(c(0, 4, 10, 6), 2)
  expect_warning(r <- relRisk(m, conf.level = 0.95, method = "use-or"),
                 "positive cell counts")
  expect_equal(unname(r), c(0, NA, NA))

  r <- relRisk(m, conf.level = 0.95, method = "use-or", delta = 0.5)
  expect_true(all(is.finite(r)))
  expect_lt(r[["lci"]], r[["uci"]])

  # the estimate itself stays uncorrected for use-or
  expect_identical(r[["est"]], 0)

  # with positive cells delta changes the use-or interval as documented
  mm <- t(cbind(c(9, 20), c(41, 29)))
  expect_false(isTRUE(all.equal(
    relRisk(mm, conf.level = 0.95, method = "use-or"),
    relRisk(mm, conf.level = 0.95, method = "use-or", delta = 0.5))))
})


test_that("arguments in ... that reach nothing are rejected", {
  m <- matrix(c(78, 50, 1422, 950), nrow = 2)
  expect_error(relRisk(m, conf.level = 0.95, metod = "wald"), "Unused arguments")
})
