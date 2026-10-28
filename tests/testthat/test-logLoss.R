test_that("logLoss computes binary log loss", {

  resp <- c(0, 0, 1, 1)
  pred <- c(0.1, 0.4, 0.35, 0.8)

  expected <- -mean(resp * log(pred) + (1 - resp) * log(1 - pred))

  expect_equal(logLoss(pred, resp), expected)
})


test_that("logLoss leaves interior predictions untouched", {

  # both terms reduce to log(0.7)
  expect_equal(logLoss(c(0.3, 0.7), c(0, 1)), -log(0.7))

  # relabelling both response and prediction is a symmetry of the loss
  resp <- c(0, 0, 1, 1)
  pred <- c(0.1, 0.4, 0.35, 0.8)

  expect_equal(logLoss(pred, resp), logLoss(1 - pred, 1 - resp))
})


test_that("logLoss is zero for effectively perfect predictions", {

  ll <- logLoss(c(0, 1), c(0, 1))

  expect_gt(ll, 0)
  expect_equal(ll, 0, tolerance = 1e-12)
})


test_that("logLoss penalizes confident wrong predictions", {

  resp <- c(0, 1)

  expect_gt(
    logLoss(c(0.99, 0.01), resp),
    logLoss(c(0.60, 0.40), resp)
  )
})


test_that("logLoss supports custom clipping", {

  expect_equal(
    logLoss(c(1, 0), c(0, 1), eps = 1e-6),
    -log(1e-6)
  )

  # the cap applies to both tails
  expect_equal(
    logLoss(c(0, 0), c(1, 1), eps = 1e-6),
    logLoss(c(1, 1), c(0, 0), eps = 1e-6)
  )
})


test_that("logLoss validates the response type", {

  # a factor passes %in% via character coercion and would silently
  # produce NA through Ops.factor
  expect_error(
    logLoss(c(0.2, 0.8), factor(c("0", "1"))),
    "numeric or logical",
    fixed = TRUE
  )

  expect_error(
    logLoss(c(0.2, 0.8), c("0", "1")),
    "numeric or logical",
    fixed = TRUE
  )

  # logical responses are accepted
  expect_equal(
    logLoss(c(0.2, 0.8), c(FALSE, TRUE)),
    logLoss(c(0.2, 0.8), c(0, 1))
  )
})


test_that("logLoss validates inputs", {

  expect_error(
    logLoss(c(0.2), c(0, 1)),
    "same length",
    fixed = TRUE
  )

  expect_error(
    logLoss(numeric(), numeric()),
    "must not be empty",
    fixed = TRUE
  )

  expect_error(
    logLoss(c(0.2, NA), c(0, 1)),
    "must not contain missing",
    fixed = TRUE
  )

  expect_error(
    logLoss(c(0.2, 0.8), c(0, 2)),
    "binary",
    fixed = TRUE
  )

  expect_error(
    logLoss(c(0.2, Inf), c(0, 1)),
    "finite",
    fixed = TRUE
  )

  expect_error(
    logLoss(c("a", "b"), c(0, 1)),
    "must be numeric",
    fixed = TRUE
  )

  expect_error(
    logLoss(c(-0.1, 0.8), c(0, 1)),
    "probabilities in [0, 1]",
    fixed = TRUE
  )

  expect_error(
    logLoss(c(0.2, 1.1), c(0, 1)),
    "probabilities in [0, 1]",
    fixed = TRUE
  )
})


test_that("logLoss validates eps", {

  for (bad in list(0, 0.5, -1, NA_real_, c(1e-6, 1e-6), "a", NULL))
    expect_error(
      logLoss(c(0.2, 0.8), c(0, 1), eps = bad),
      "'eps'",
      fixed = TRUE
    )
})

test_that("the default method needs 'ref'", {
  expect_error(logLoss(c(0.2, 0.8)), "'ref' must be provided")
})
