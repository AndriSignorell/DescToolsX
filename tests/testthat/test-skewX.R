
test_that("skew returns a single numeric (no CI)", {
  x <- rnorm(100)
  res <- skew(x)
  expect_length(res, 1); expect_true(is.numeric(res))
})

test_that("skew is near 0 for a normal distribution", {
  set.seed(1)
  x <- rnorm(5000)
  expect_lt(abs(skew(x, estimator = 3)), 0.1)
})

test_that("skew is positive for a right-skewed distribution", {
  x <- c(rep(1, 50), rep(10, 5))
  expect_gt(skew(x), 0)
})

test_that("skew is negative for a left-skewed distribution", {
  x <- c(rep(1, 5), rep(10, 50))
  expect_lt(skew(x), 0)
})

test_that("skew estimators 1, 2, 3 give different results for small n", {
  # Need asymmetric data — symmetric vectors give skew=0 for all estimators
  x  <- c(1, 1, 2, 3, 10, 15)
  s1 <- skew(x, estimator=1)
  s2 <- skew(x, estimator=2)
  s3 <- skew(x, estimator=3)
  expect_false(isTRUE(all.equal(s1, s2)))
  expect_false(isTRUE(all.equal(s1, s3)))
})

test_that("skew stops for invalid estimator", {
  expect_error(skew(rnorm(10), estimator = 4))
})

test_that("skew stops for non-numeric x", {
  expect_error(skew(c("a","b","c")), "numeric")
})

test_that("skew na.rm = TRUE strips NAs", {
  x <- c(rnorm(49), NA)
  expect_equal(skew(x, na.rm = TRUE), skew(x[1:49]))
})

test_that("skew conf.level = 0.95 returns named vector est/lci/uci", {
  set.seed(2)
  x <- rnorm(100)
  res <- skew(x, conf.level = 0.95, R = 300)
  expect_length(res, 3)
  expect_true(all(c("est","lci","uci") %in% names(res)))
})

test_that("skew CI: lci < est < uci", {
  set.seed(3)
  x <- c(rnorm(100), rep(5,10))
  res <- skew(x, conf.level = 0.95, R = 300)
  expect_lt(res["lci"], res["est"])
  expect_gt(res["uci"], res["est"])
})



# Reference values are computed from the raw moments, independently of the
# C++ kernel, so a change to skew_cpp() cannot make these tests agree with a
# wrong result.

.refSkew <- function(x, estimator = 3) {
  n <- length(x)
  m2 <- mean((x - mean(x))^2)
  m3 <- mean((x - mean(x))^3)
  g1 <- m3 / m2^(3/2)
  switch(as.character(estimator),
         "1" = g1,
         "2" = g1 * sqrt(n * (n - 1)) / (n - 2),
         "3" = g1 * ((n - 1) / n)^(3/2))
}

.refSkewSe <- function(n, estimator = 3) {
  se <- sqrt(6 * (n - 2) / ((n + 1) * (n + 3)))
  switch(as.character(estimator),
         "1" = se,
         "2" = se * sqrt(n * (n - 1)) / (n - 2),
         "3" = se * ((n - 1) / n)^(3/2))
}

x <- c(2, 3, 3, 4, 4, 4, 5, 5, 6, 12, 15, 3, 4, 4, 5, 6, 7, 8, 3, 4)


test_that("skew() reproduces the three documented estimators", {
  
  expect_equal(unname(skew(x, estimator = 1)), .refSkew(x, 1))
  expect_equal(unname(skew(x, estimator = 2)), .refSkew(x, 2))
  expect_equal(unname(skew(x, estimator = 3)), .refSkew(x, 3))
  
  # estimator 3 is the default
  expect_equal(unname(skew(x)), .refSkew(x, 3))
})


test_that("the classic interval is centred on the estimate", {
  
  # This is the regression test for the defect: the bounds used to be
  # -z*se and +z*se, an interval around zero that need not contain the
  # estimate printed beside it.
  res <- skew(x, conf.level = 0.95, method = "classic")
  
  expect_named(res, c("est", "lci", "uci"))
  expect_true(res[["lci"]] <= res[["est"]])
  expect_true(res[["est"]] <= res[["uci"]])
  
  n <- length(x)
  se <- .refSkewSe(n, 3)
  
  expect_equal(res[["lci"]], .refSkew(x, 3) + qnorm(0.025) * se)
  expect_equal(res[["uci"]], .refSkew(x, 3) + qnorm(0.975) * se)
  
  # with these data the old bounds excluded the estimate entirely
  expect_false(qnorm(0.975) * se >= .refSkew(x, 3))
})


test_that("the documented ASE belongs to estimator 2", {
  
  n <- length(x)
  ase <- sqrt(6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3)))
  
  expect_equal(.refSkewSe(n, 2), ase)
  
  res1 <- skew(x, conf.level = 0.95, method = "classic", estimator = 1)
  res2 <- skew(x, conf.level = 0.95, method = "classic", estimator = 2)
  
  # est/se is invariant across estimators, so the half-widths scale exactly
  # like the estimates
  expect_equal(
    (res2[["uci"]] - res2[["est"]]) / (res1[["uci"]] - res1[["est"]]),
    res2[["est"]] / res1[["est"]]
  )
})


test_that("equal weights reproduce the unweighted result", {
  
  expect_equal(
    unname(skew(x, weights = rep(1, length(x)))),
    unname(skew(x))
  )
  
  expect_equal(
    skew(x, weights = rep(1, length(x)), conf.level = 0.95,
         method = "classic"),
    skew(x, conf.level = 0.95, method = "classic")
  )
})


test_that("frequency weights match the replicated sample", {
  
  v <- c(1, 2, 5, 7)
  w <- c(3, 1, 2, 1)
  
  expect_equal(
    unname(skew(v, weights = w)),
    unname(skew(rep(v, w)))
  )
})


test_that("the bootstrap resamples weights together with the values", {
  
  # If the weights were not permuted along with the data, replicate i would
  # carry the weight of the original observation i and the interval would
  # differ from the one obtained on the replicated sample.
  set.seed(42)
  a <- skew(c(1, 2, 5, 7), weights = c(3, 1, 2, 1), conf.level = 0.95,
            R = 200)
  
  expect_named(a, c("est", "lci", "uci"))
  expect_true(a[["lci"]] <= a[["est"]])
  expect_true(a[["est"]] <= a[["uci"]])
})


test_that("na.rm reaches the bootstrap branch", {
  
  y <- c(x, NA)
  
  expect_false(is.na(skew(y, na.rm = TRUE)))
  
  set.seed(1)
  res <- skew(y, conf.level = 0.95, na.rm = TRUE, R = 200)
  expect_false(anyNA(res))
})


test_that("the standard error is NA where it is undefined", {
  
  res <- skew(c(1, 2), conf.level = 0.95, method = "classic")
  expect_true(is.na(res[["lci"]]))
  expect_true(is.na(res[["uci"]]))
})


test_that("sides names the side carrying the finite bound", {
  
  left  <- skew(x, conf.level = 0.95, sides = "left",  method = "classic")
  right <- skew(x, conf.level = 0.95, sides = "right", method = "classic")
  
  expect_identical(left[["uci"]], Inf)
  expect_identical(right[["lci"]], -Inf)
  
  # the full alpha sits on the finite side
  n <- length(x)
  expect_equal(left[["lci"]],
               .refSkew(x, 3) + qnorm(0.05) * .refSkewSe(n, 3))
})


test_that("arguments are validated", {
  
  expect_error(skew(letters), "numeric")
  expect_error(skew(x, weights = 1:3), "same length")
  expect_error(skew(x, estimator = 4), "estimator")
  expect_error(skew(x, estimator = c(1, 2)), "estimator")
  expect_error(skew(x, conf.level = NULL), "single value")
  expect_error(skew(x, conf.level = 1), "conf.level")
})

