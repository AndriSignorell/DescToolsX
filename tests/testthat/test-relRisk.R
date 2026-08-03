
# Reference values for the score interval were obtained by solving the
# Koopman score equation numerically (constrained MLE of p2 from
#   (n1+n2) t p^2 - (t (n1+x2) + n2 + x1) p + (x1+x2) = 0,
#  chi2(t) = (x1-n1 t p)^2/(n1 t p (1-t p)) + (x2-n2 p)^2/(n2 p (1-p)) = z^2),
# independently of the closed-form cubic used in the implementation. Both
# agree to 3e-08 relative for all 9801 tables with n1, n2 in
# {4,5,10,25,60} and 0 < x1 < n1, 0 < x2 < n2.

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
  # the cubic has a root on the boundary p2 = 1 here; the old root selection
  # returned (0.700, 0.938), i.e. a lower bound ABOVE the estimate
  m <- matrix(c(2, 5, 3, 0), nrow = 2)
  r <- relRisk(m, conf.level = 0.95)
  expect_equal(unname(r), c(0.4, 0.1176207742, 0.9378042349), tolerance = 1e-6)

  r <- relRisk(matrix(c(4, 10, 6, 0), nrow = 2), conf.level = 0.95)
  expect_equal(unname(r), c(0.4, 0.1681803297, 0.6873262303), tolerance = 1e-6)

  r <- relRisk(matrix(c(9, 5, 1, 0), nrow = 2), conf.level = 0.95)
  expect_equal(unname(r), c(0.9, 0.5958499732, 1.6170723132), tolerance = 1e-6)
})


test_that("relRisk() score interval always brackets the estimate", {

  for (n1 in c(5L, 10L, 12L)) {
    for (n2 in c(5L, 10L, 12L)) {
      for (x1 in 0:n1) {
        for (x2 in 0:n2) {
          m <- matrix(c(x1, x2, n1 - x1, n2 - x2), nrow = 2)
          r <- relRisk(m, conf.level = 0.95)
          lab <- sprintf("x1=%d x2=%d n1=%d n2=%d", x1, x2, n1, n2)

          # x1 == x2 == 0 leaves the estimate itself undefined (0/0)
          if (!(x1 == 0 && x2 == 0))
            expect_false(anyNA(r), label = paste("no NA:", lab))
          expect_false(anyNA(r[c("lci", "uci")]),
                       label = paste("no NA in bounds:", lab))
          expect_true(r[["lci"]] <= r[["uci"]] + 1e-12,
                      label = paste("lci <= uci:", lab))

          if (is.finite(r[["est"]]))
            expect_true(r[["lci"]] <= r[["est"]] + 1e-8 &&
                          r[["est"]] <= r[["uci"]] + 1e-8,
                        label = paste("lci <= est <= uci:", lab))
        }
      }
    }
  }
})


test_that("relRisk() handles a first row without non-events (x1 == n1)", {

  # regression: x1 == n1 puts the constrained MLE on p1 = 1. The cubic's middle
  # root then no longer gives the lower bound - the interval came out far too
  # narrow - and where that root equals (x2 + n1)/(n1 + n2) the bound formula
  # collapsed to 0/0. Reference values solve the score equation numerically.
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
               c(0.3037489456, 0.9242779886), tolerance = 1e-7)

  o <- relRisk(mm, conf.level = 0.95, method = "use-or")
  expect_named(o, c("est", "lci", "uci"))
  expect_true(o[["lci"]] < o[["est"]] && o[["est"]] < o[["uci"]])

  # documented caveat: delta enters the standard error only, so a zero cell
  # still collapses the Wald interval onto the degenerate point estimate
  d <- relRisk(matrix(c(0, 4, 10, 6), nrow = 2), conf.level = 0.95,
               method = "wald")
  expect_equal(unname(d), c(0, 0, 0))
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
