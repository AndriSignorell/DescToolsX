test_that("hodgesLehmann returns a single numeric (one-sample)", {
  x <- c(2, 4, 6, 8, 10)
  res <- hodgesLehmann(x)
  expect_length(res, 1)
  expect_true(is.numeric(res))
})

test_that("hodgesLehmann equals the median for a symmetric distribution", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(hodgesLehmann(x), 3, tolerance = 1e-6)
})

test_that("hodgesLehmann one-sample matches wilcox.test pseudomedian (no ties)", {
  set.seed(1)
  x <- rnorm(30, mean = 5)
  hl  <- hodgesLehmann(x)
  wt  <- wilcox.test(x, conf.int = TRUE, exact = FALSE)$estimate
  expect_equal(hl, unname(wt), tolerance = 0.01)
})

test_that("hodgesLehmann two-sample returns a numeric", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(3, 4, 5, 6, 7)
  res <- hodgesLehmann(x, y)
  expect_length(res, 1)
  expect_true(is.numeric(res))
})

test_that("hodgesLehmann two-sample shift = 0 when groups are identical", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(hodgesLehmann(x, x), 0, tolerance = 1e-9)
})

test_that("hodgesLehmann returns NA for NA input without na.rm", {
  expect_true(is.na(hodgesLehmann(c(1, 2, NA))))
})

test_that("hodgesLehmann na.rm = TRUE strips NAs before estimation", {
  x <- c(1, 2, 3, NA, 5)
  res <- hodgesLehmann(x, na.rm = TRUE)
  expect_false(is.na(res))
})

test_that("hodgesLehmann conf.level returns named vector est/lci/uci", {
  set.seed(42)
  x <- rnorm(50, mean = 5)
  res <- hodgesLehmann(x, conf.level = 0.95, R = 300)
  expect_length(res, 3)
  expect_named(res, c("est", "lci", "uci"))
})

test_that("hodgesLehmann CI: lci < est < uci", {
  set.seed(42)
  x <- rnorm(50, mean = 5)
  res <- hodgesLehmann(x, conf.level = 0.95, R = 300)
  expect_lt(res["lci"], res["est"])
  expect_gt(res["uci"], res["est"])
})

test_that("hodgesLehmann stops for non-numeric x", {
  expect_error(hodgesLehmann(c("a", "b")), "numeric")
})



test_that("hodgesLehmann does not modify its arguments", {
  
  # NumericVector is a view on the caller's SEXP, so the in-place
  # std::sort() in hlqest_cpp()/hl2qest_cpp() reordered the user's vector. Only
  # na.omit() upstream produced a copy, i.e. the vector was mangled
  # exactly in the ordinary case.
  # w <- v would share the SEXP, so a mutation would hit both and pass;
  # serialize() gives an independent snapshot
  v <- c(3, 1, 2, 5, 4)
  before <- serialize(v, NULL)
  hodgesLehmann(v)
  expect_identical(serialize(v, NULL), before)

  a <- c(9, 7, 8)
  b <- c(2, 1, 3)
  a0 <- serialize(a, NULL); b0 <- serialize(b, NULL)
  hodgesLehmann(a, b)
  expect_identical(serialize(a, NULL), a0)
  expect_identical(serialize(b, NULL), b0)
})


test_that("one-sample estimate is the median of the Walsh averages", {
  
  x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
  
  walsh <- outer(x, x, "+") / 2
  expected <- median(walsh[upper.tri(walsh, diag = TRUE)])
  
  expect_equal(hodgesLehmann(x), expected)
  
  # n = 4: 10 Walsh averages, so the estimate falls between two of them
  x2 <- c(4, 1, 7, 2)
  w2 <- outer(x2, x2, "+") / 2
  expect_equal(hodgesLehmann(x2),
               median(w2[upper.tri(w2, diag = TRUE)]))
})


test_that("two-sample estimate is the median of the pairwise differences", {
  
  set.seed(1)
  x <- round(rnorm(12, 5), 2)
  y <- round(rnorm(9, 3), 2)
  
  expect_equal(hodgesLehmann(x, y), median(outer(x, y, "-")))
  
  # and it is NOT generally the difference of the medians
  expect_false(isTRUE(all.equal(hodgesLehmann(x, y),
                                median(x) - median(y))))
})


test_that("the estimate is exact under heavy ties", {
  
  # the partition step falls into its tie branch here, which is where
  # wilcox.test()'s pseudomedian goes wrong
  x <- c(rep(2, 15), rep(5, 15), rep(9, 5))
  w <- outer(x, x, "+") / 2
  expect_equal(hodgesLehmann(x), median(w[upper.tri(w, diag = TRUE)]))
  
  expect_equal(hodgesLehmann(rep(7, 20)), 7)
})


test_that("small and degenerate inputs", {
  
  expect_equal(hodgesLehmann(5), 5)
  expect_equal(hodgesLehmann(c(1, 4)), 2.5)
  
  # empty samples are empty data, not a calling error (design rules, NA policy)
  expect_identical(hodgesLehmann(numeric(0)), NA_real_)
  expect_identical(hodgesLehmann(1:5, numeric(0)), NA_real_)
  
  expect_equal(hodgesLehmann(c(2, 4, 6), 1), 3)   # m > 1, n == 1
  expect_equal(hodgesLehmann(5, c(1, 3, 5)), 2)   # m == 1, n > 1
})


test_that("the point estimate does not depend on R's RNG", {
  
  # pivot selection is arbitrary but the result is an exact order
  # statistic, so two different seeds must agree - and the routine must
  # not consume R's stream at all
  x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30, 4.2, 0.1)
  
  set.seed(1);  a <- hodgesLehmann(x)
  set.seed(99); b <- hodgesLehmann(x)
  expect_identical(a, b)
  
  set.seed(1)
  before <- .Random.seed
  hodgesLehmann(x)
  expect_identical(.Random.seed, before)
})


test_that("missing values follow the documented contract", {
  
  x <- c(1, 2, NA, 4)
  
  expect_true(is.na(hodgesLehmann(x)))
  expect_equal(hodgesLehmann(x, na.rm = TRUE), hodgesLehmann(c(1, 2, 4)))
  
  res <- hodgesLehmann(x, conf.level = 0.95)
  expect_named(res, c("est", "lci", "uci"))
  expect_true(all(is.na(res)))
})


test_that("the bootstrap interval brackets the estimate and honours sides", {
  
  set.seed(42)
  x <- rnorm(40, 10)
  
  two   <- hodgesLehmann(x, conf.level = 0.95, R = 499)
  expect_named(two, c("est", "lci", "uci"))
  expect_lt(two[["lci"]], two[["est"]])
  expect_gt(two[["uci"]], two[["est"]])
  
  set.seed(42)
  left  <- hodgesLehmann(x, conf.level = 0.95, sides = "left",  R = 499)
  set.seed(42)
  right <- hodgesLehmann(x, conf.level = 0.95, sides = "right", R = 499)
  
  expect_identical(unname(left[["uci"]]), Inf)
  expect_identical(unname(right[["lci"]]), -Inf)
  expect_true(is.finite(left[["lci"]]))
  expect_true(is.finite(right[["uci"]]))
})


test_that("two-sample with conf.level is refused, not silently wrong", {
  expect_error(hodgesLehmann(1:10, 1:8, conf.level = 0.95),
               "one-sample case")
})




test_that("missing values give NA of the documented shape", {
  
  x <- c(1.83, 0.50, 1.62, 2.48, NA)
  y <- c(0.878, 0.647, NaN)
  
  expect_identical(hodgesLehmann(x), NA_real_)
  expect_identical(hodgesLehmann(x[-5], y), NA_real_)
  expect_identical(hodgesLehmann(c(1, NaN, 3)), NA_real_)
  
  ci <- hodgesLehmann(x, conf.level = 0.95)
  expect_identical(ci, c(est = NA_real_, lci = NA_real_, uci = NA_real_))
})


test_that("na.rm cleans x and y separately, they are not pairs", {
  
  x <- c(1.83, 0.50, NA, 2.48, 1.68)
  y <- c(0.878, 0.647, 0.598)          # unequal length: used to stop
  
  expect_equal(hodgesLehmann(x, y, na.rm = TRUE),
               hodgesLehmann(x[-3], y))
  
  # equal length: the valid y[3] must survive the NA in x[3]
  y5 <- c(0.878, 0.647, 0.598, 2.05, 1.06)
  expect_equal(hodgesLehmann(x, y5, na.rm = TRUE),
               hodgesLehmann(x[-3], y5))
})


test_that("empty samples give NA, not an error", {
  
  expect_identical(hodgesLehmann(numeric(0)), NA_real_)
  expect_identical(hodgesLehmann(c(NA, NA), na.rm = TRUE), NA_real_)
  expect_identical(hodgesLehmann(1:3, numeric(0)), NA_real_)
  expect_identical(hodgesLehmann(1:3, c(NA, NaN), na.rm = TRUE), NA_real_)
  expect_identical(hodgesLehmann(c(NA, NA), na.rm = TRUE, conf.level = 0.9),
                   c(est = NA_real_, lci = NA_real_, uci = NA_real_))
})


test_that("invalid arguments are not masked by missing data", {
  
  x <- c(1, NA, 3)
  
  expect_error(hodgesLehmann(c("a", NA)), "numeric")
  expect_error(hodgesLehmann(x, c("a", NA)), "numeric")
  expect_error(hodgesLehmann(x, conf.level = 1.5))
  expect_error(hodgesLehmann(x, conf.level = 0.95, sides = "up"))
  expect_error(hodgesLehmann(x, 1:3, conf.level = 0.95), "one-sample")
})


test_that("two-sample search terminates under ties (former endless loop)", {

  # the initial row bounds froze the search: am = -0.5, sq = l = 6, sm = 15
  x <- c(0, 0, 1, 1, 1)
  y <- c(0, 0, 0, 1, 1, 1)
  expect_equal(hodgesLehmann(x, y), median(outer(x, y, "-")))

  set.seed(3)
  for (i in 1:200) {
    x <- sample(0:2, sample(2:15, 1), replace = TRUE)
    y <- sample(0:2, sample(2:15, 1), replace = TRUE)
    expect_equal(hodgesLehmann(x, y), median(outer(x, y, "-")))
  }
})


test_that("no overflow when the result is finite", {
  expect_equal(hodgesLehmann(c(1e308, 1e308)), 1e308)
  expect_equal(hodgesLehmann(1e308, 1e308), 0)
  expect_equal(hodgesLehmann(c(-.Machine$double.xmax, 0, .Machine$double.xmax)), 0)
})


test_that("infinite values: order statistics, NaN where undefined", {
  expect_equal(hodgesLehmann(c(1, 2, 3, Inf)), 2.75)
  expect_equal(hodgesLehmann(c(1, 2, Inf), c(0, 1, 2)), 1)

  # differences: four -Inf, two Inf; this one used to hit the iteration
  # cap, since the midrange step produced (-Inf + Inf) / 2
  expect_identical(hodgesLehmann(c(-Inf, -Inf, Inf), c(0, 0)), -Inf)
  expect_identical(hodgesLehmann(c(1, Inf, Inf), 0), Inf)

  expect_identical(hodgesLehmann(c(-Inf, 1, Inf)), NaN)      # Inf - Inf pair
  expect_identical(hodgesLehmann(c(1, Inf), c(0, Inf)), NaN)
  expect_identical(hodgesLehmann(c(-Inf, Inf), 0), NaN)      # middle -Inf, Inf

  # against brute force, all combinations of a small infinite alphabet
  set.seed(5)
  ref <- function(d) if (anyNA(d)) NaN else median(d)
  for (i in 1:200) {
    x <- sample(c(-Inf, 0:3, Inf), sample(1:7, 1), replace = TRUE)
    y <- sample(c(-Inf, 0:3, Inf), sample(1:7, 1), replace = TRUE)
    w <- outer(x, x, "+") / 2
    expect_identical(hodgesLehmann(x), ref(w[upper.tri(w, diag = TRUE)]))
    expect_identical(hodgesLehmann(x, y), ref(outer(x, y, "-")))
  }
})


test_that("argument validation", {
  x <- c(1.83, 0.50, 1.62, 2.48, 1.68)

  expect_error(hodgesLehmann(x, na.rm = NA))
  expect_error(hodgesLehmann(x, na.rm = "yes"))
  expect_error(hodgesLehmann(x, na.rm = c(TRUE, FALSE)))

  # NaN is an invalid level, not a synonym for NA
  expect_error(hodgesLehmann(x, conf.level = NaN))

  # one-sided levels must stay positive after 1 - 2 * (1 - conf.level)
  expect_error(hodgesLehmann(x, conf.level = 0.5, sides = "left"), "0.5")
  expect_error(hodgesLehmann(x, conf.level = 0.3, sides = "right"), "0.5")

  expect_error(hodgesLehmann(x, conf.level = 0.95, type = "stud"), "stud")
})


test_that("degenerate bootstrap cases keep the documented shape", {

  # point mass: collapsed interval, no warning
  set.seed(1)
  res <- expect_silent(hodgesLehmann(rep(7, 20), conf.level = 0.95, R = 199))
  expect_identical(res, c(est = 7, lci = 7, uci = 7))

  set.seed(1)
  res <- hodgesLehmann(rep(7, 20), conf.level = 0.95, sides = "left", R = 199)
  expect_identical(res, c(est = 7, lci = 7, uci = Inf))

  # one observation: named NA bounds with warning
  expect_warning(res <- hodgesLehmann(7, conf.level = 0.95), "single")
  expect_identical(res, c(est = 7, lci = NA_real_, uci = NA_real_))

  # non-finite replicates
  set.seed(1)
  expect_warning(res <- hodgesLehmann(c(1, 2, 3, Inf), conf.level = 0.95,
                                      R = 199), "finite")
  expect_named(res, c("est", "lci", "uci"))
  expect_true(all(is.na(res[c("lci", "uci")])))
})
