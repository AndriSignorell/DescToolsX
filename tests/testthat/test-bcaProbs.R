test_that(".bcaZ0 locates the estimate within the replicates", {

  t <- seq(0, 1, length.out = 101)

  # exactly half of the replicates below the estimate
  expect_equal(.bcaZ0(t, t0 = 0.5), qnorm(0.5), tolerance = 1e-8)

  # a quarter below
  expect_equal(.bcaZ0(t, t0 = 0.25), qnorm((25 + 0.5) / 101), tolerance = 1e-12)
})


test_that(".bcaZ0 counts ties half, and stays finite at the edges", {

  # a discrete statistic: many replicates land exactly on the estimate
  t <- c(rep(0, 60), rep(0.5, 40))

  # 0 below, 60 tied  ->  30 / 100
  expect_equal(.bcaZ0(t, t0 = 0), qnorm(0.30), tolerance = 1e-12)

  # the case that used to give -Inf: nothing below, nothing tied
  z <- .bcaZ0(rep(1, 500), t0 = 0)
  expect_true(is.finite(z))
  expect_equal(z, qnorm(0.5 / 500), tolerance = 1e-12)

  # ... and the mirror image
  expect_true(is.finite(.bcaZ0(rep(0, 500), t0 = 1)))
})


test_that(".bcaProbs is the identity when there is nothing to correct", {

  p <- c(0.025, 0.975)
  expect_equal(.bcaProbs(p, z0 = 0, a = 0), p, tolerance = 1e-12)
})


test_that(".bcaProbs shifts in the expected direction and stays ordered", {

  p <- c(0.025, 0.975)

  # a positive bias correction moves both probabilities up
  up <- .bcaProbs(p, z0 = 0.3, a = 0)
  expect_true(all(up > p))
  expect_true(up[1] < up[2])

  # acceleration alone keeps the order too
  acc <- .bcaProbs(p, z0 = 0, a = 0.05)
  expect_true(acc[1] < acc[2])
  expect_true(all(acc > 0 & acc < 1))
})


test_that(".bcaProbs leaves the open side of a one-sided interval alone", {

  # this is the construction that must not be adjusted: qnorm(1) is Inf
  left  <- .bcaProbs(c(0.05, 1), z0 = 0.2, a = 0.03)
  right <- .bcaProbs(c(0, 0.95), z0 = 0.2, a = 0.03)

  expect_equal(left[2], 1)
  expect_equal(right[1], 0)

  expect_true(is.finite(left[1]) && left[1] > 0 && left[1] < 1)
  expect_true(is.finite(right[2]) && right[2] > 0 && right[2] < 1)

  # and the finite side is adjusted exactly as it would be on its own
  expect_equal(left[1], .bcaProbs(c(0.05, 0.5), z0 = 0.2, a = 0.03)[1])
})


test_that(".bcaProbs falls back to the percentile bounds instead of failing", {

  p <- c(0.025, 0.975)

  # denominator 1 - a * (z0 + z) turns non-positive
  expect_warning(res <- .bcaProbs(p, z0 = 0, a = 0.6), "not defined")
  expect_equal(res, p)

  # the acceleration could not be estimated
  expect_warning(res <- .bcaProbs(p, z0 = 0, a = NA_real_), "not defined")
  expect_equal(res, p)

  expect_warning(res <- .bcaProbs(p, z0 = Inf, a = 0), "not defined")
  expect_equal(res, p)
})


test_that("the adjusted probabilities are usable as quantile() input", {

  # the point of computing probabilities rather than indices: whatever
  # comes out of here, quantile() cannot read past the end of the vector
  set.seed(7)
  t <- rbeta(999, 2, 5)

  for (z0 in c(-1, 0, 0.4))
    for (a in c(-0.1, 0, 0.1)) {
      p  <- .bcaProbs(c(0.025, 0.975), z0 = z0, a = a)
      ci <- quantile(t, probs = p, names = FALSE)
      expect_true(all(ci >= min(t) & ci <= max(t)))
      expect_true(ci[1] <= ci[2])
    }
})
