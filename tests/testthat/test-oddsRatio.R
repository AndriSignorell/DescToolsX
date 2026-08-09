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

  # confint.glm() prints "Waiting for profiling to be done..." as a
  # message, so silence is the wrong bar here - the point is that a
  # two-sided request warns about nothing
  expect_no_warning(suppressMessages(oddsRatio(fit, method = "profile")))
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
