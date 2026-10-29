
# Conditional (exact / mid-p) reference values solve the noncentral
# hypergeometric equations directly with 50-digit arithmetic (mpmath),
# independently of the implementation and of fisher.test(). Tables are given
# by rows: c(n11, n12, n21, n22).

tab <- matrix(c(10, 20,
                 5, 30), nrow = 2)

byRow <- function(v) matrix(v, 2, byrow = TRUE)


test_that("all three table methods treat sides the same way", {

  # REGRESSION: they did not. "wald" was right, "exact" mapped
  # sides = "left" to fisher.test(alternative = "less") - whose interval
  # has a finite UPPER bound, i.e. the wrong end - and "mid-p" ignored
  # sides outright and always returned the two-sided interval.
  for (m in c("wald", "exact", "mid-p")) {

    two   <- oddsRatio(tab, conf.level = 0.95, method = m)
    left  <- oddsRatio(tab, conf.level = 0.95, method = m, sides = "left")
    right <- oddsRatio(tab, conf.level = 0.95, method = m, sides = "right")

    expect_named(left, c("est", "lci", "uci"), info = m)

    # an odds ratio is bounded below by 0 and unbounded above
    expect_equal(left[["uci"]], Inf, info = m)
    expect_equal(right[["lci"]], 0, info = m)

    # the estimate never depends on the sidedness
    expect_equal(left[["est"]],  two[["est"]], info = m)
    expect_equal(right[["est"]], two[["est"]], info = m)

    # a one-sided bound carries the whole alpha, so it is tighter
    expect_true(left[["lci"]]  >= two[["lci"]], info = m)
    expect_true(right[["uci"]] <= two[["uci"]], info = m)
  }
})


test_that("the one-sided bound equals the two-sided one at the adjusted level", {

  for (m in c("wald", "exact", "mid-p")) {

    left  <- oddsRatio(tab, conf.level = 0.95, method = m, sides = "left")
    right <- oddsRatio(tab, conf.level = 0.95, method = m, sides = "right")
    two   <- oddsRatio(tab, conf.level = 0.90, method = m)

    expect_equal(left[["lci"]],  two[["lci"]], info = m)
    expect_equal(right[["uci"]], two[["uci"]], info = m)
  }
})


test_that("for this table the exact interval is the widest, Wald the narrowest", {

  # not a general property - Wald can be wider than mid-p for small cells
  w <- oddsRatio(tab, conf.level = 0.95, method = "wald")
  e <- oddsRatio(tab, conf.level = 0.95, method = "exact")
  p <- oddsRatio(tab, conf.level = 0.95, method = "mid-p")

  expect_gt(diff(e[c("lci", "uci")]), diff(p[c("lci", "uci")]))
  expect_gt(diff(p[c("lci", "uci")]), diff(w[c("lci", "uci")]))
})


test_that("conf.level is validated and one-sided below 0.5 refused", {

  expect_error(oddsRatio(tab, conf.level = c(0.9, 0.95)), "conf.level")
  expect_error(oddsRatio(tab, conf.level = NULL), "conf.level")
  expect_error(oddsRatio(tab, conf.level = NaN), "conf.level")
  expect_error(oddsRatio(tab, conf.level = 0), "conf.level")

  expect_error(oddsRatio(tab, conf.level = 0.4, sides = "left"), "0.5")
  expect_silent(oddsRatio(tab, conf.level = 0.4))

  # matched even when no interval is requested
  expect_error(oddsRatio(tab, sides = "links"), "two.sided")
  expect_length(oddsRatio(tab), 1L)
})


# ---------------------------------------------- conditional methods ----

test_that("exact and mid-p match independent high-precision references", {

  ref <- list(
    list(tab = c(10, 5, 20, 30),
         exact = c(2.94833379148105, 0.777066659309722, 12.740766787576),
         midp  = c(2.91188151330854, 0.879526435696008, 10.859855828667)),
    list(tab = c(12, 7, 5, 9),
         exact = c(2.97678052390209, 0.599265965836366, 16.6473722738734),
         midp  = c(2.94485070601084, 0.704761951666939, 13.6985338035144)),
    # small odds ratio: relative precision is what matters here
    list(tab = c(1, 200, 30, 40),
         exact = c(0.0068226412681393, 0.000162652663239799, 0.043421877211511),
         midp  = c(0.00774495254521974, 0.000322058781014558, 0.0375494329296033))
  )

  for (r in ref) {
    m <- byRow(r$tab)
    e <- oddsRatio(m, conf.level = 0.95, method = "exact")
    p <- oddsRatio(m, conf.level = 0.95, method = "mid-p")
    expect_equal(unname(e) / r$exact, rep(1, 3), tolerance = 1e-9,
                 info = paste(r$tab, collapse = ","))
    expect_equal(unname(p) / r$midp, rep(1, 3), tolerance = 1e-9,
                 info = paste(r$tab, collapse = ","))
  }

  # the 90% mid-p interval, via a one-sided call at 95%
  m <- byRow(c(10, 5, 20, 30))
  expect_equal(oddsRatio(m, conf.level = 0.95, method = "mid-p",
                         sides = "left")[["lci"]],
               1.06262148113547, tolerance = 1e-9)
})


test_that("exact agrees with fisher.test() up to fisher.test()'s tolerance", {

  # fisher.test() roots with uniroot()'s absolute default tolerance (~1e-4)
  for (v in list(c(12, 7, 5, 9), c(10, 5, 20, 30), c(3, 9, 8, 2))) {
    m <- byRow(v)
    f <- fisher.test(m)
    e <- oddsRatio(m, conf.level = 0.95, method = "exact")
    expect_equal(unname(e), unname(c(f$estimate, f$conf.int)),
                 tolerance = 1e-3, info = paste(v, collapse = ","))
  }

  # ... which costs fisher.test() 4% at a lower bound of 1.6e-4
  m <- byRow(c(1, 200, 30, 40))
  expect_gt(abs(fisher.test(m)$conf.int[1] / 0.000162652663239799 - 1), 0.01)
})


test_that("conditional methods keep the informative bound with a zero cell", {

  # REGRESSION: mid-p died in uniroot() ("values at end points not of
  # opposite sign") on every table with a zero cell.

  # n11 at the top of its support: estimate and upper bound are Inf
  m <- byRow(c(10, 5, 0, 7))
  e <- oddsRatio(m, conf.level = 0.95, method = "exact")
  p <- oddsRatio(m, conf.level = 0.95, method = "mid-p")
  expect_equal(unname(e), c(Inf, 1.81804185196074, Inf), tolerance = 1e-9)
  expect_equal(unname(p), c(Inf, 2.4633283272161,  Inf), tolerance = 1e-9)

  # n11 at the bottom: estimate and lower bound are 0
  m <- byRow(c(0, 5, 10, 7))
  e <- oddsRatio(m, conf.level = 0.95, method = "exact")
  p <- oddsRatio(m, conf.level = 0.95, method = "mid-p")
  expect_equal(unname(e), c(0, 0, 1.08319082927695),  tolerance = 1e-9)
  expect_equal(unname(p), c(0, 0, 0.789876634276939), tolerance = 1e-9)

  # two zero cells
  p <- oddsRatio(byRow(c(3, 0, 0, 4)), conf.level = 0.95, method = "mid-p")
  expect_equal(unname(p), c(Inf, 1.40960948663846, Inf), tolerance = 1e-9)

  # point estimates without an interval
  expect_identical(oddsRatio(byRow(c(10, 5, 0, 7)), method = "mid-p"), Inf)
  expect_identical(oddsRatio(byRow(c(0, 5, 10, 7)), method = "exact"), 0)
})


test_that("each method reports its own point estimate", {

  m <- byRow(c(12, 7, 5, 9))
  expect_equal(oddsRatio(m, method = "wald"), (12 * 9) / (7 * 5))
  expect_equal(oddsRatio(m, method = "exact"), 2.97678052390209,
               tolerance = 1e-9)
  expect_equal(oddsRatio(m, method = "mid-p"), 2.94485070601084,
               tolerance = 1e-9)
})


test_that("conditional methods stay fast and accurate for large tables", {

  m <- matrix(c(500, 300, 1e5 - 500, 1e5 - 300), 2)
  for (method in c("exact", "mid-p")) {
    r <- oddsRatio(m, conf.level = 0.95, method = method)
    expect_true(all(is.finite(r)), info = method)
    expect_true(r[["lci"]] < r[["est"]] && r[["est"]] < r[["uci"]],
                info = method)
  }
  # agrees with the large-sample Wald interval to two digits here
  w <- oddsRatio(m, conf.level = 0.95)
  expect_equal(unname(r), unname(w), tolerance = 1e-2)
})


test_that("the root search refuses to invent an open bound", {
  expect_error(.rootLog(function(l) -1, increasing = TRUE), "Could not bracket")
  expect_equal(.rootLog(function(l) l - 3, increasing = TRUE), 3,
               tolerance = 1e-10)
  expect_equal(.rootLog(function(l) -l - 700, increasing = FALSE), -700,
               tolerance = 1e-10)
})


# ---------------------------------------------------------- wald ----

test_that("Wald corrects zero cells only when asked to", {

  x <- matrix(c(10, 0, 5, 7), 2)

  # REGRESSION: 0.5 was added silently to every cell, contrary to the
  # documented estimate of 0 or Inf
  expect_identical(oddsRatio(x), Inf)
  expect_warning(r <- oddsRatio(x, conf.level = 0.95), "positive cell counts")
  expect_equal(unname(r), c(Inf, NA, NA))

  # one-sided output still opens the natural end
  expect_warning(r <- oddsRatio(x, conf.level = 0.95, sides = "left"),
                 "positive cell counts")
  expect_equal(unname(r), c(Inf, NA, Inf))

  xh  <- x + 0.5
  est <- xh[1, 1] * xh[2, 2] / (xh[1, 2] * xh[2, 1])
  se  <- sqrt(sum(1 / xh))
  expect_equal(oddsRatio(x, delta = 0.5), est)
  expect_equal(unname(oddsRatio(x, conf.level = 0.95, delta = 0.5)),
               c(est, exp(log(est) + c(-1, 1) * qnorm(0.975) * se)))

  # delta is validated always, but used by "wald" only
  expect_equal(oddsRatio(tab, method = "exact", delta = 0.5),
               oddsRatio(tab, method = "exact"))
  for (v in list(-1, NA_real_, Inf, c(0, 1), "0"))
    expect_error(oddsRatio(tab, delta = v), "delta")
})


test_that("the Wald interval matches its closed form", {
  logEst <- log(10 * 30 / (5 * 20))
  se <- sqrt(1 / 10 + 1 / 5 + 1 / 20 + 1 / 30)
  expect_equal(unname(oddsRatio(tab, conf.level = 0.95)),
               exp(logEst + c(0, -1, 1) * qnorm(0.975) * se))
})


# ---------------------------------------------------------------- glm ----

fit <- glm(vs ~ am, data = mtcars, family = binomial)


test_that("the glm method opens the side at 0 and Inf", {

  two   <- oddsRatio(fit)
  left  <- oddsRatio(fit, sides = "left")
  right <- oddsRatio(fit, sides = "right")

  expect_s3_class(two, "OddsRatio")
  expect_equal(two$coefficients$est, left$coefficients$est)

  expect_true(all(left$coefficients$uci == Inf))
  expect_true(all(right$coefficients$lci == 0))

  expect_true(all(left$coefficients$lci >= two$coefficients$lci))
  expect_true(all(right$coefficients$uci <= two$coefficients$uci))

  # left(gamma) reads the same end as two.sided(2*gamma - 1)
  expect_equal(left$coefficients$lci,
               oddsRatio(fit, conf.level = 0.90)$coefficients$lci)
})


test_that("the glm method works for a single-coefficient model", {

  # regression against the vapply/mapply trap: with one row the bounds
  # must still come back as a column, not as a length-2 vector
  fit1 <- glm(vs ~ 1, data = mtcars, family = binomial)
  res  <- oddsRatio(fit1, sides = "left")

  expect_equal(nrow(res$coefficients), 1L)
  expect_equal(res$coefficients$uci, Inf)
})


test_that("profile intervals are two-sided and say so", {

  skip_if_not(getRversion() >= "4.4.0" || requireNamespace("MASS", quietly = TRUE))

  expect_warning(res <- oddsRatio(fit, method = "profile", sides = "left"),
                 "two-sided")

  # the object records what was computed, not what was asked for
  expect_equal(res$sides, "two.sided")
  expect_true(all(is.finite(res$coefficients$uci)))

  # the profiling message of confint.glm() is muffled inside oddsRatio(),
  # so a two-sided request is completely silent
  expect_silent(oddsRatio(fit, method = "profile"))
})


test_that("the glm method accepts conf.level = NA and keeps the columns", {

  res <- oddsRatio(fit, conf.level = NA)

  expect_true(all(c("lci", "uci") %in% names(res$coefficients)))
  expect_true(all(is.na(res$coefficients$lci)))
  expect_equal(res$coefficients$est, exp(coef(fit)), ignore_attr = TRUE)

  out <- capture.output(print(res))
  expect_true(any(grepl("no confidence interval", out, fixed = TRUE)))
  expect_false(any(grepl("NA%", out, fixed = TRUE)))
})


test_that("the glm method validates conf.level like every other function", {

  expect_error(oddsRatio(fit, conf.level = c(0.9, 0.95)), "conf.level")
  expect_error(oddsRatio(fit, conf.level = 0), "conf.level")
  expect_error(oddsRatio(fit, conf.level = 0.4, sides = "right"), "0.5")

  # an lm has no oddsRatio method and falls through to the default one;
  # it used to die there on "Argument 'x' must be numeric", which points
  # at the wrong end of the problem
  expect_error(oddsRatio(lm(mpg ~ am, data = mtcars)), "binomial glm")

  # the inherits() guard inside oddsRatio.glm is unreachable through S3
  # dispatch - it only fires on a direct call, which is what it is for
  expect_error(DescToolsX:::oddsRatio.glm(lm(mpg ~ am, data = mtcars)), "glm")

  # a gaussian glm does dispatch here
  expect_error(oddsRatio(glm(mpg ~ am, data = mtcars)), "binomial")
})


test_that("the exponentiated intercept is the baseline odds", {

  res <- oddsRatio(fit)
  i   <- match("(Intercept)", res$coefficients$term)

  # documented as such: it is not an odds ratio, and this pins the value
  # so the column order cannot silently shift
  expect_equal(res$coefficients$est[i], exp(coef(fit)[["(Intercept)"]]))
  expect_equal(res$coefficients$logEst[i], coef(fit)[["(Intercept)"]])
})


test_that("the glm method refuses other families and prints its table", {
  pfit <- glm(carb ~ mpg, data = mtcars, family = poisson)
  expect_error(oddsRatio(pfit), "binomial")
  fit <- glm(vs ~ mpg, data = mtcars, family = binomial)
  or  <- oddsRatio(fit, conf.level = 0.95)
  out <- capture.output(res <- print(or))
  expect_identical(res, or)
  expect_true(any(grepl("Odds Ratios (95% two.sided CI, method = wald)", out,
                        fixed = TRUE)))
  expect_true(any(grepl("mpg", out, fixed = TRUE)))
})


# ------------------------------------------------------- input checks ----

test_that("oddsRatio.default validates the table", {
  expect_error(oddsRatio(lm(mpg ~ wt, data = mtcars)), "2x2 table or a binomial glm")
  expect_error(oddsRatio(matrix(letters[1:4], 2)), "numeric")
  expect_error(oddsRatio(matrix(c(1, NA, 2, 3), 2)), "missing")
  expect_error(oddsRatio(array(1:8, c(2, 2, 2))), "2x2 matrix or table")
  expect_error(oddsRatio(1:4), "supply 'y'")
  expect_error(oddsRatio(matrix(1:9, 3)), "2x2 matrix")
  expect_error(oddsRatio(matrix(c(1, -1, 2, 3), 2)), "non-negative")
  expect_error(oddsRatio(matrix(c(1, 1.5, 2, 3), 2)), "integer counts")
  expect_error(oddsRatio(matrix(c(0, 3, 0, 4), 2)), "positive totals")

  # REGRESSION: Inf reported "integer counts" (Inf %% 1 is NaN)
  expect_error(oddsRatio(matrix(c(1, Inf, 2, 3), 2)), "finite counts")

  # REGRESSION: a constant outcome passed - Wald returned NaN or, with
  # its old silent correction, 1; the conditional methods died in
  # fisher.test(). The odds ratio is undefined there.
  for (m in c("wald", "exact", "mid-p"))
    expect_error(oddsRatio(matrix(c(0, 0, 3, 4), 2), method = m),
                 "Columns of 'x' must contain positive totals", info = m)
})


test_that("arguments that reach nothing are not dropped silently", {
  # 'interval' was removed with the adaptive search; it must not vanish
  expect_error(oddsRatio(tab, method = "mid-p", interval = c(0, 10)),
               "Unused arguments")
})


test_that("two vectors are cross-tabulated first", {
  x <- c("a", "a", "b", "b", "a", "b", "a")
  y <- c("u", "v", "u", "v", "u", "v", "v")
  expect_equal(oddsRatio(x, y, conf.level = 0.95),
               oddsRatio(table(x, y), conf.level = 0.95))
  expect_equal(oddsRatio(x, y, conf.level = 0.95, method = "mid-p",
                         useNA = "no"),
               oddsRatio(table(x, y), conf.level = 0.95, method = "mid-p"))
})
