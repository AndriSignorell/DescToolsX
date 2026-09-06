test_that("logLoss computes binary log loss", {

  resp <- c(0, 0, 1, 1)
  pred <- c(0.1, 0.4, 0.35, 0.8)

  expected <- -mean(resp * log(pred) + (1 - resp) * log(1 - pred))

  expect_equal(logLoss(resp, pred), expected)
})


test_that("logLoss leaves interior predictions untouched", {

  # both terms reduce to log(0.7)
  expect_equal(logLoss(c(0, 1), c(0.3, 0.7)), -log(0.7))

  # relabelling both response and prediction is a symmetry of the loss
  resp <- c(0, 0, 1, 1)
  pred <- c(0.1, 0.4, 0.35, 0.8)

  expect_equal(logLoss(resp, pred), logLoss(1 - resp, 1 - pred))
})


test_that("logLoss is zero for effectively perfect predictions", {

  ll <- logLoss(c(0, 1), c(0, 1))

  expect_gt(ll, 0)
  expect_equal(ll, 0, tolerance = 1e-12)
})


test_that("logLoss penalizes confident wrong predictions", {

  resp <- c(0, 1)

  expect_gt(
    logLoss(resp, c(0.99, 0.01)),
    logLoss(resp, c(0.60, 0.40))
  )
})


test_that("logLoss supports custom clipping", {

  expect_equal(
    logLoss(c(0, 1), c(1, 0), eps = 1e-6),
    -log(1e-6)
  )

  # the cap applies to both tails
  expect_equal(
    logLoss(c(1, 1), c(0, 0), eps = 1e-6),
    logLoss(c(0, 0), c(1, 1), eps = 1e-6)
  )
})


test_that("logLoss accepts a fitted glm", {

  m <- glm(am ~ hp + wt, data = mtcars, family = binomial)

  expect_equal(
    logLoss(m),
    logLoss(m$y, predict(m, type = "response"))
  )

  # for ungrouped Bernoulli data the saturated log-likelihood is zero,
  # hence deviance == 2 * n * logLoss -- an independent reference
  expect_equal(logLoss(m), m$deviance / (2 * nrow(mtcars)))
})


test_that("logLoss handles na.exclude in fitted glm", {

  # regression test: predict() pads its result via napredict() under
  # na.exclude, $fitted.values does not and stays aligned with $y
  d <- mtcars
  d$hp[c(2, 5)] <- NA

  m <- glm(
    am ~ hp + wt,
    data = d,
    family = binomial,
    na.action = na.exclude
  )

  expect_length(m$y, nrow(mtcars) - 2L)

  expect_equal(
    logLoss(m),
    m$deviance / (2 * length(m$y))
  )
})


test_that("logLoss rejects unsupported models", {

  expect_error(
    logLoss(lm(mpg ~ wt, data = mtcars)),
    "only for 'glm'",
    fixed = TRUE
  )

  expect_error(
    logLoss(glm(mpg ~ wt, data = mtcars)),
    "got \"gaussian\"",
    fixed = TRUE
  )

  # quasibinomial has no full likelihood, hence no log loss
  expect_error(
    logLoss(glm(am ~ wt, data = mtcars, family = quasibinomial)),
    "got \"quasibinomial\"",
    fixed = TRUE
  )

  grp <- data.frame(succ = c(3, 7), fail = c(7, 3), x = c(0, 1))

  expect_error(
    logLoss(glm(cbind(succ, fail) ~ x, data = grp, family = binomial)),
    "prior weights",
    fixed = TRUE
  )
})


test_that("logLoss validates the response type", {

  # a factor passes %in% via character coercion and would silently
  # produce NA through Ops.factor
  expect_error(
    logLoss(factor(c("0", "1")), c(0.2, 0.8)),
    "numeric or logical",
    fixed = TRUE
  )

  expect_error(
    logLoss(c("0", "1"), c(0.2, 0.8)),
    "numeric or logical",
    fixed = TRUE
  )

  # logical responses are accepted
  expect_equal(
    logLoss(c(FALSE, TRUE), c(0.2, 0.8)),
    logLoss(c(0, 1), c(0.2, 0.8))
  )
})


test_that("logLoss validates inputs", {

  expect_error(
    logLoss(c(0, 1), c(0.2)),
    "same length",
    fixed = TRUE
  )

  expect_error(
    logLoss(numeric(), numeric()),
    "must not be empty",
    fixed = TRUE
  )

  expect_error(
    logLoss(c(0, 1), c(0.2, NA)),
    "must not contain missing",
    fixed = TRUE
  )

  expect_error(
    logLoss(c(0, 2), c(0.2, 0.8)),
    "binary",
    fixed = TRUE
  )

  expect_error(
    logLoss(c(0, 1), c(0.2, Inf)),
    "finite",
    fixed = TRUE
  )

  expect_error(
    logLoss(c(0, 1), c("a", "b")),
    "must be numeric",
    fixed = TRUE
  )

  expect_error(
    logLoss(c(0, 1), c(-0.1, 0.8)),
    "probabilities in [0, 1]",
    fixed = TRUE
  )

  expect_error(
    logLoss(c(0, 1), c(0.2, 1.1)),
    "probabilities in [0, 1]",
    fixed = TRUE
  )
})


test_that("logLoss validates eps", {

  for (bad in list(0, 0.5, -1, NA_real_, c(1e-6, 1e-6), "a", NULL))
    expect_error(
      logLoss(c(0, 1), c(0.2, 0.8), eps = bad),
      "'eps'",
      fixed = TRUE
    )
})
