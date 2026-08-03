
# Reference implementation: the classical O(n^2) formula from Hollander,
# Wolfe & Chicken, scaled by 30 so that perfect dependence is 1. Slow,
# but it does not share a single line with the Fenwick version - which is
# the point of having it here.
.hoeffDref <- function(x, y) {
  n <- length(x)
  R <- vapply(seq_len(n), function(i) sum(x <= x[i]), numeric(1))
  S <- vapply(seq_len(n), function(i) sum(y <= y[i]), numeric(1))
  Q <- vapply(seq_len(n), function(i) sum(x <= x[i] & y <= y[i]), numeric(1))

  D1 <- sum((Q - 1) * (Q - 2))
  D2 <- sum((R - 1) * (R - 2) * (S - 1) * (S - 2))
  D3 <- sum((R - 2) * (S - 2) * (Q - 1))

  30 * ((n - 2) * (n - 3) * D1 + D2 - 2 * (n - 2) * D3) /
    (n * (n - 1) * (n - 2) * (n - 3) * (n - 4))
}


test_that("hoeffdingD matches the classical formula", {

  set.seed(1)
  for (n in c(10, 25, 60)) {
    x <- rnorm(n)
    y <- rnorm(n)
    expect_equal(hoeffdingD(x, y), .hoeffDref(x, y), tolerance = 1e-10,
                 label = paste0("n = ", n))
  }

  # and for a nonlinear relationship, where D earns its keep
  x <- runif(80, -3, 3)
  y <- x^2 + rnorm(80, sd = 0.05)
  expect_equal(hoeffdingD(x, y), .hoeffDref(x, y), tolerance = 1e-10)
})


test_that("the scale is the conventional one: perfect dependence is 1", {

  # documented as [-1/60, 1/30], but the C++ multiplies by 30
  expect_equal(hoeffdingD(1:50, (1:50)^3), 1)
  expect_equal(hoeffdingD(1:50, rev(1:50)), 1)     # antitone, also 1

  set.seed(4)
  x <- rnorm(300); y <- rnorm(300)
  expect_lt(abs(hoeffdingD(x, y)), 0.02)           # independent -> ~0
})


test_that("ties in x are jittered too, not just ties in y", {

  # x has ties, y does not. jitter = TRUE suppresses the warning, so if
  # only y were jittered the ties in x would go through silently.
  set.seed(5)
  x <- rep(1:10, each = 5)
  y <- rnorm(50)

  expect_warning(hoeffdingD(x, y), "Ties")
  expect_no_warning(d <- hoeffdingD(x, y, jitter = TRUE, seed = 1))
  expect_true(is.finite(d))

  # jittering must actually change the tie structure: with x fully tied
  # the unjittered result depends on the arbitrary order() tie-break
  x2 <- rep(1, 50)
  expect_error(hoeffdingD(x2, y, jitter = TRUE), "no variation")
})


test_that("a supplied seed does not disturb the caller's RNG stream", {

  set.seed(123)
  before <- .Random.seed

  x <- rep(1:10, each = 3)
  y <- rep(1:15, times = 2)
  hoeffdingD(x, y, jitter = TRUE, seed = 42)

  expect_identical(.Random.seed, before)

  # ... and the seed still makes the result reproducible
  d1 <- hoeffdingD(x, y, jitter = TRUE, seed = 42)
  d2 <- hoeffdingD(x, y, jitter = TRUE, seed = 42)
  expect_equal(d1, d2)
})


test_that("missing and non-finite values are refused", {

  x <- c(rnorm(10), NA)
  y <- rnorm(11)

  # order()/rank() sort NAs to the end, so this used to return a number
  expect_error(hoeffdingD(x, y), "missing values")
  expect_error(hoeffdingD(y, x), "missing values")
  expect_error(hoeffdingD(c(rnorm(10), Inf), y), "infinite")

  expect_error(hoeffdingD(letters[1:10], 1:10), "numeric")
})


test_that("the permutation guard rejects malformed input", {

  expect_error(DescToolsX:::hoeffdingD_cpp(c(0, 1, 2, 3, 3)), "permutation")
  expect_error(DescToolsX:::hoeffdingD_cpp(c(0, 1, 2, 3, -1)), "permutation")
  expect_error(DescToolsX:::hoeffdingD_cpp(c(0, 1, 2, 3, 4.5)), "permutation")
  expect_error(DescToolsX:::hoeffdingD_cpp(c(0, 1, 2, 3)), "n must be")
})


test_that("the int64 threshold is where the comment says it is", {

  skip_on_cran()

  # The 14081 boundary has a margin of exactly two observations, so it
  # is worth asserting rather than trusting: signed overflow is
  # undefined behaviour and would yield a plausible-looking number, not
  # an error. The monotone permutation is the worst case for the running
  # sum. Doubles carry 53 bits, so the check is done on the log scale to
  # avoid asserting a bound with a tool that cannot represent it.
  logPeakAt <- function(n) {
    i <- seq_len(n) - 1          # a = i, b = c = i for the identity perm
    terms <- (i * (i - 1) / 2) * (i * (i - 1)) -
      ((i - 1) * (i - 1)) * i * (n - 2) +
      (i * (i - 1) / 2) * (n - 2) * (n - 3)
    log2(max(abs(cumsum(terms))))
  }

  # 9.2195e18 vs an int64 ceiling of 9.2234e18
  expect_lt(logPeakAt(14081), 63)

  # and one step past the documented limit it really does overflow, so
  # the constant is not merely conservative
  expect_gt(logPeakAt(14083), 63)
})


test_that("basic contract", {

  set.seed(1)
  x <- rnorm(100); y <- rnorm(100)

  d <- hoeffdingD(x, y)
  expect_length(d, 1)
  expect_true(is.numeric(d))
  expect_null(names(d))

  expect_error(hoeffdingD(1:10, 1:5), "same length")
  expect_error(hoeffdingD(1:4, 1:4), "5")
})
