
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


test_that("the two engines agree on data without ties", {
  
  set.seed(1)
  x <- rnorm(120)
  y <- x^2 + rnorm(120)
  
  expect_equal(hoeffdingD(x, y),
               hoeffdingD(x, y, engine = "exact"),
               tolerance = 1e-10)
  
  # and on a monotone relation, and on noise
  set.seed(2)
  a <- runif(80); b <- runif(80)
  expect_equal(hoeffdingD(a, b), hoeffdingD(a, b, engine = "exact"),
               tolerance = 1e-10)
  expect_equal(hoeffdingD(a, exp(a)), hoeffdingD(a, exp(a), engine = "exact"),
               tolerance = 1e-10)
})


test_that("the exact engine reproduces the reference values", {
  
  # 1 under perfect monotone and antitone dependence
  expect_equal(hoeffdingD(1:50, (1:50)^3, engine = "exact"), 1)
  expect_equal(hoeffdingD(1:50, 50:1, engine = "exact"), 1)
  expect_equal(hoeffdingD(1:20, (1:20)^3, engine = "exact"), 1)
  
  # the documented minimum over all permutations at n = 7
  perms <- function(v) {
    if (length(v) == 1L) return(list(v))
    do.call(c, lapply(seq_along(v),
                      function(i) lapply(perms(v[-i]), function(p) c(v[i], p))))
  }
  d <- vapply(perms(1:7),
              function(p) hoeffdingD(1:7, p, engine = "exact"), numeric(1))
  expect_equal(min(d), -0.2619048, tolerance = 1e-6)
  
  # approximately zero under independence
  set.seed(3)
  m <- mean(replicate(200, hoeffdingD(1:20, sample(20), engine = "exact")))
  expect_lt(abs(m), 0.05)
})


test_that("the exact engine handles ties, the fast one warns about them", {
  
  set.seed(4)
  x <- rnorm(100)
  y <- round(x^2 + rnorm(100), 1)   # ties in y
  
  # the fast engine points at both remedies
  expect_warning(hoeffdingD(x, y), "exact")
  
  # the exact one just answers
  expect_silent(d <- hoeffdingD(x, y, engine = "exact"))
  expect_true(is.finite(d))
  
  # heavy ties: the two engines must now DIFFER, otherwise the exact
  # branch is not doing anything
  xr <- round(x, 0)
  yr <- round(y, 0)
  fast  <- suppressWarnings(hoeffdingD(xr, yr))
  exact <- hoeffdingD(xr, yr, engine = "exact")
  expect_false(isTRUE(all.equal(fast, exact)))
})


test_that("jitter is refused for the exact engine rather than ignored", {
  
  set.seed(5)
  x <- rnorm(30); y <- round(rnorm(30), 1)
  
  expect_error(hoeffdingD(x, y, engine = "exact", jitter = TRUE), "jitter")
  
  # and it still works for the fast one
  expect_silent(hoeffdingD(x, y, jitter = TRUE, seed = 1))
})


test_that("the shared argument checks apply to both engines", {
  
  x <- rnorm(20); y <- rnorm(20)
  
  for (e in c("fast", "exact")) {
    expect_error(hoeffdingD(x, y[-1], engine = e), "same length")
    expect_error(hoeffdingD(c(x[-1], NA), y, engine = e), "missing")
    expect_error(hoeffdingD(c(x[-1], Inf), y, engine = e), "infinite")
    expect_error(hoeffdingD(x[1:4], y[1:4], engine = e), "5 observations")
    expect_error(hoeffdingD(as.character(x), y, engine = e), "numeric")
  }
  
  expect_error(hoeffdingD(x, y, engine = "quick"), "fast")
  expect_error(hoeffdingD(x, y, jitter = NA), "jitter")
})


test_that("neither engine disturbs the caller's random stream", {
  
  set.seed(6)
  x <- rnorm(40); y <- round(rnorm(40), 1)
  
  set.seed(99); before <- runif(1)
  
  set.seed(99)
  invisible(hoeffdingD(x, y, engine = "exact"))
  expect_equal(runif(1), before)
  
  set.seed(99)
  invisible(hoeffdingD(x, y, jitter = TRUE, seed = 7))
  expect_equal(runif(1), before)
})


# ------------------------------------------------------------ test ----

test_that("output = 'test' returns a usable htest", {
  
  set.seed(11)
  x <- rnorm(60)
  y <- x^2 + rnorm(60)
  
  h <- hoeffdingD(x, y, output = "test", R = 199, seed = 1)
  
  expect_s3_class(h, "htest")
  expect_named(h$statistic, "D")
  expect_equal(unname(h$statistic), hoeffdingD(x, y))
  expect_equal(unname(h$parameter[["R"]]), 199)
  expect_equal(unname(h$parameter[["n"]]), 60)
  expect_match(h$method, "permutation")
  expect_match(h$data.name, "x and y")
  
  # prints without error
  expect_output(print(h), "Hoeffding")
  
  # the default output is unchanged and costs no permutations
  expect_length(hoeffdingD(x, y), 1L)
  expect_null(names(hoeffdingD(x, y)))
})


test_that("the P value cannot be zero and respects its own floor", {
  
  # perfect monotone dependence: no permutation can beat it, so the P
  # value is the smallest the design allows, 1/(R+1) - not 0
  h <- hoeffdingD(1:40, (1:40)^3, output = "test", R = 99, seed = 2)
  expect_equal(h$p.value, 1 / 100)
  
  h2 <- hoeffdingD(1:40, (1:40)^3, output = "test", R = 999, seed = 2)
  expect_equal(h2$p.value, 1 / 1000)
  
  # and it always lies in (0, 1]
  set.seed(12)
  for (i in 1:5) {
    hh <- hoeffdingD(rnorm(30), rnorm(30), output = "test", R = 99)
    expect_gt(hh$p.value, 0)
    expect_lte(hh$p.value, 1)
  }
})


test_that("the test finds dependence that correlation misses", {
  
  set.seed(13)
  x <- runif(150, -1, 1)
  y <- x^2 + rnorm(150, sd = 0.05)
  
  expect_gt(cor.test(x, y)$p.value, 0.05)
  expect_lt(hoeffdingD(x, y, output = "test", R = 499, seed = 3)$p.value, 0.01)
})


test_that("the P value is roughly uniform under independence", {
  
  skip_on_cran()
  
  set.seed(14)
  p <- replicate(200,
                 hoeffdingD(rnorm(25), rnorm(25),
                            output = "test", R = 99)$p.value)
  
  # with 200 draws the Monte Carlo error on a proportion is about 0.03,
  # so this is a smoke test for gross miscalibration, not a fine one
  expect_lt(abs(mean(p <= 0.10) - 0.10), 0.08)
  expect_lt(abs(mean(p <= 0.50) - 0.50), 0.12)
})


test_that("both engines can produce the test, and ties survive the exact one", {
  
  set.seed(15)
  x <- rnorm(40)
  y <- round(x^2 + rnorm(40), 1)
  
  he <- hoeffdingD(x, y, engine = "exact", output = "test", R = 99, seed = 4)
  expect_s3_class(he, "htest")
  expect_equal(unname(he$statistic), hoeffdingD(x, y, engine = "exact"))
  expect_match(he$method, "exact")
  
  hf <- suppressWarnings(
    hoeffdingD(x, y, output = "test", R = 99, seed = 4))
  expect_match(hf$method, "fast")
})


test_that("R is validated and the seed still leaves the stream alone", {
  
  set.seed(16)
  x <- rnorm(30); y <- rnorm(30)
  
  expect_error(hoeffdingD(x, y, output = "test", R = 0), "'R'")
  expect_error(hoeffdingD(x, y, output = "test", R = 99.5), "'R'")
  expect_error(hoeffdingD(x, y, output = "test", R = -1), "'R'")
  expect_error(hoeffdingD(x, y, output = "test", seed = "a"), "seed")
  expect_error(hoeffdingD(x, y, output = "quick"), "def")
  
  # a supplied seed makes the test reproducible ...
  a <- hoeffdingD(x, y, output = "test", R = 99, seed = 5)$p.value
  b <- hoeffdingD(x, y, output = "test", R = 99, seed = 5)$p.value
  expect_equal(a, b)
  
  # ... without disturbing the caller's stream
  set.seed(99); before <- runif(1)
  set.seed(99)
  invisible(hoeffdingD(x, y, output = "test", R = 99, seed = 5))
  expect_equal(runif(1), before)
  
  # and without a seed it follows set.seed() like everything else
  set.seed(21); p1 <- hoeffdingD(x, y, output = "test", R = 99)$p.value
  set.seed(21); p2 <- hoeffdingD(x, y, output = "test", R = 99)$p.value
  expect_equal(p1, p2)
})

