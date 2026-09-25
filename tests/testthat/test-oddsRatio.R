tab <- matrix(c(10, 20,
                 5, 30), nrow = 2)


test_that("all three table methods treat sides the same way", {

  # REGRESSION: they did not. "wald" was right, "exact" mapped
  # sides = "left" to fisher.test(alternative = "less") - whose interval
  # has a finite UPPER bound, i.e. the wrong end - and "midp" ignored
  # sides outright and always returned the two-sided interval.
  for (m in c("wald", "exact", "midp")) {

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

  for (m in c("wald", "exact", "midp")) {

    left  <- oddsRatio(tab, conf.level = 0.95, method = m, sides = "left")
    right <- oddsRatio(tab, conf.level = 0.95, method = m, sides = "right")
    two   <- oddsRatio(tab, conf.level = 0.90, method = m)

    expect_equal(left[["lci"]],  two[["lci"]], info = m)
    expect_equal(right[["uci"]], two[["uci"]], info = m)
  }
})


test_that("the exact interval is the widest, the Wald interval the narrowest", {

  w <- oddsRatio(tab, conf.level = 0.95, method = "wald")
  e <- oddsRatio(tab, conf.level = 0.95, method = "exact")
  p <- oddsRatio(tab, conf.level = 0.95, method = "midp")

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


# Review 26.09.2026: input checks, the zero-cell correction, print --------

test_that("oddsRatio.default validates the table", {
  expect_error(oddsRatio(lm(mpg ~ wt, data = mtcars)), "2x2 table or a binomial glm")
  expect_error(oddsRatio(matrix(letters[1:4], 2)), "numeric")
  expect_error(oddsRatio(matrix(c(1, NA, 2, 3), 2)), "missing")
  expect_error(oddsRatio(array(1:8, c(2, 2, 2))), "must be a matrix")
  expect_error(oddsRatio(matrix(1:9, 3)), "2x2 matrix")
  expect_error(oddsRatio(matrix(c(1, -1, 2, 3), 2)), "non-negative")
  expect_error(oddsRatio(matrix(c(1, 1.5, 2, 3), 2)), "integer counts")
  expect_error(oddsRatio(matrix(c(0, 3, 0, 4), 2)), "positive totals")
})

test_that("two vectors are cross-tabulated first", {
  x <- c("a", "a", "b", "b", "a", "b", "a")
  y <- c("u", "v", "u", "v", "u", "v", "v")
  expect_equal(oddsRatio(x, y, conf.level = 0.95),
               oddsRatio(table(x, y), conf.level = 0.95))
})

test_that("the Wald method adds 0.5 to every cell when one is zero", {
  x  <- matrix(c(10, 0, 5, 7), 2)
  xh <- x + 0.5
  est <- xh[1, 1] * xh[2, 2] / (xh[1, 2] * xh[2, 1])
  se  <- sqrt(sum(1 / xh))
  expect_equal(unname(oddsRatio(x, method = "wald")), est)
  expect_equal(unname(oddsRatio(x, method = "wald", conf.level = 0.95)),
               c(est, exp(log(est) + c(-1, 1) * qnorm(0.975) * se)))
})

test_that("exact and mid-p give point estimates without conf.level", {
  x <- matrix(c(12, 5, 7, 9), 2)
  expect_equal(unname(oddsRatio(x, method = "exact")),
               unname(fisher.test(x)$estimate))
  mp <- oddsRatio(x, method = "midp")
  expect_length(mp, 1L)
  expect_true(is.finite(mp) && mp > 0)
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
