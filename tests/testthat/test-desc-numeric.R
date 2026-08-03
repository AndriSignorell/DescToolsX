
test_that("meanAD is the mean absolute deviation, not a truncated one", {

  # The C++ used unqualified abs() on a double. Without a
  # "using namespace std" that resolves to the C int abs(int) and
  # truncates: every deviation below 1 in magnitude became 0.
  ref <- function(z) mean(abs(z - mean(z)))

  # deviations well above 1 - the old code was biased low but nonzero
  x <- c(28.4, 31.2, 29.9, 35.1, 27.6, 33.3, 30.0, 26.8)
  expect_equal(desc(x)$meanAD, ref(x))

  # deviations below 1 - the old code returned exactly 0
  p <- c(0.12, 0.35, 0.48, 0.51, 0.63, 0.29, 0.77, 0.41)
  expect_equal(desc(p)$meanAD, ref(p))
  expect_gt(desc(p)$meanAD, 0)

  # ties are weighted by their frequency
  y <- c(rep(1.2, 10), rep(3.7, 5), 9.1)
  expect_equal(desc(y)$meanAD, ref(y))
})


test_that("the power sums agree with a direct computation", {

  set.seed(1)
  x <- round(rnorm(200, 50, 8), 1)   # deliberately with ties

  ps <- DescToolsX:::n_pow_sum_cpp(sort(x))
  d  <- x - mean(x)

  expect_equal(ps$mean, mean(x))
  expect_equal(ps$sum1, sum(abs(d)))
  expect_equal(ps$sum2, sum(d^2))
  expect_equal(ps$sum3, sum(d^3))
  expect_equal(ps$sum4, sum(d^4))

  expect_equal(ps$unique, length(unique(x)))
  expect_equal(ps$zero, sum(x == 0))
})


test_that("sd, var, skew and kurt match the standalone functions", {

  set.seed(2)
  x <- rgamma(150, shape = 2, scale = 3)
  d <- desc(x)

  expect_equal(d$sd, sd(x))
  expect_equal(d$var, var(x))
  expect_equal(d$mean, mean(x))
  expect_equal(unname(d$quant[["median"]]), median(x))

  # type-3 skewness and kurtosis, written out independently
  n  <- length(x)
  m2 <- mean((x - mean(x))^2)
  m3 <- mean((x - mean(x))^3)
  m4 <- mean((x - mean(x))^4)

  expect_equal(d$skew, (m3 / m2^1.5) * ((n - 1) / n)^(3 / 2))
  expect_equal(d$kurt, (m4 / m2^2) * (1 - 1 / n)^2 - 3)
})


test_that("the smallest and largest values come back in order with counts", {

  x <- c(rep(2, 3), 5, 5, 7, 11, 13, 17, 19, rep(23, 4))

  ps <- DescToolsX:::n_pow_sum_cpp(sort(x))

  expect_equal(ps$small_val, c(2, 5, 7, 11, 13))
  expect_equal(ps$small_freq, c(3L, 2L, 1L, 1L, 1L))

  expect_equal(ps$large_val, c(23, 19, 17, 13, 11))   # descending
  expect_equal(ps$large_freq, c(4L, 1L, 1L, 1L, 1L))

  # fewer than five distinct values: both vectors shrink, no padding
  ps2 <- DescToolsX:::n_pow_sum_cpp(c(1, 1, 2))
  expect_length(ps2$small_val, 2L)
  expect_equal(ps2$small_val, c(1, 2))
  expect_equal(ps2$large_val, c(2, 1))
})


test_that("the C++ kernel refuses input it cannot order", {

  # a NaN key breaks the strict weak ordering std::map requires, which is
  # undefined behaviour rather than a wrong number
  expect_error(DescToolsX:::n_pow_sum_cpp(c(1, 2, NaN)), "NA or NaN")
  expect_error(DescToolsX:::n_pow_sum_cpp(c(1, 2, NA_real_)), "NA or NaN")
  expect_error(DescToolsX:::n_pow_sum_cpp(numeric(0)), "empty")
})


test_that("print does not damage the object it was given", {

  set.seed(3)
  x <- rnorm(50)
  d <- desc(x)

  before <- d
  capture.output(print(d))

  # print() rewrites x$n, x$length etc. into formatted strings for its own
  # layout; that must not leak back to the caller
  expect_identical(d, before)

  # and it returns the object invisibly
  out <- withVisible(capture.output(res <- print(d)))
  expect_identical(res$n, before$n)
})


test_that("degenerate inputs do not error", {

  expect_silent(d1 <- desc(c(4, 4, 4, 4)))    # no variation
  expect_equal(d1$sd, 0)
  expect_equal(d1$meanAD, 0)
  expect_equal(d1$unique, 1)

  # a single non-missing value
  expect_silent(desc(c(7, NA, NA)))
})
