
test_that("hodgesLehmann does not modify its arguments", {

  # NumericVector is a view on the caller's SEXP, so the in-place
  # std::sort() in hlqest_cpp()/hl2qest_cpp() reordered the user's vector. Only
  # na.omit() upstream produced a copy, i.e. the vector was mangled
  # exactly in the ordinary case.
  v <- c(3, 1, 2, 5, 4)
  w <- v
  hodgesLehmann(v)
  expect_identical(v, w)

  a <- c(9, 7, 8)
  b <- c(2, 1, 3)
  a0 <- a; b0 <- b
  hodgesLehmann(a, b)
  expect_identical(a, a0)
  expect_identical(b, b0)
})


test_that("one-sample estimate is the median of the Walsh averages", {

  x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)

  walsh <- outer(x, x, "+") / 2
  expected <- median(walsh[upper.tri(walsh, diag = TRUE)])

  expect_equal(hodgesLehmann(x), expected)

  # even n, so the estimate falls between two Walsh averages
  x2 <- c(4, 1, 7, 2, 9, 3)
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

  expect_error(hodgesLehmann(numeric(0)), "at least one observation")
  expect_error(hodgesLehmann(1:5, numeric(0)), "at least one observation")

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
