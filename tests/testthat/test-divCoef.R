

library(testthat)

test_that("divCoef works for basic input", {
  set.seed(1)
  df <- matrix(runif(20), ncol = 4)
  d <- dist(matrix(rnorm(10), ncol = 2))
  
  res <- divCoef(df, d)
  
  expect_type(res, "double")
  expect_length(res, ncol(df))
  expect_true(all(res >= 0))
})

test_that("divCoef handles zero columns correctly", {
  df <- matrix(0, nrow = 5, ncol = 3)
  d <- dist(matrix(rnorm(10), ncol = 2))
  
  res <- divCoef(df, d)
  
  expect_true(all(res == 0))
})

test_that("divCoef errors on negative values", {
  df <- matrix(c(1, -1, 2, 3), ncol = 2)
  
  expect_error(divCoef(df))
})

test_that("divCoef default distance works", {
  df <- matrix(runif(20), ncol = 4)
  
  res <- divCoef(df)
  
  expect_length(res, ncol(df))
})

test_that("Normalization scales values <= 1", {
  set.seed(1)
  df <- matrix(runif(20), ncol = 4)
  d <- dist(matrix(rnorm(10), ncol = 2))
  
  res <- divCoef(df, d, normalize = TRUE)
  
  expect_true(all(res <= 1 + 1e-8))
})


test_that("Scaling df does not change result", {
  set.seed(1)
  df <- matrix(runif(20), ncol = 4)
  d <- dist(matrix(rnorm(10), ncol = 2))
  
  res1 <- divCoef(df, d)
  res2 <- divCoef(2 * df, d)
  
  expect_equal(res1, res2, tolerance = 1e-8)
})


test_that("Row permutation does not change result", {
  set.seed(1)
  df <- matrix(runif(20), ncol = 4)
  d <- dist(matrix(rnorm(10), ncol = 2))
  
  perm <- sample(nrow(df))
  
  res1 <- divCoef(df, d)
  res2 <- divCoef(df[perm, ], as.dist(as.matrix(d)[perm, perm]))
  
  expect_equal(res1, res2, tolerance = 1e-8)
})


test_that("divCoef reduces to Gini-Simpson without a distance matrix", {
  
  x <- cbind(a = c(1, 1, 1, 1), b = c(4, 0, 0, 0), d = c(2, 2, 0, 0))
  
  expect_equal(unname(divCoef(x)), c(1 - 4 * 0.25^2, 0, 0.5))
})


test_that("divCoef reports missing values instead of aborting", {
  
  x <- cbind(a = c(1, 1, NA), b = c(1, 1, 1))
  
  expect_error(divCoef(x), "missing values")
  res <- divCoef(x, na.rm = TRUE)
  expect_true(is.na(res[1]))
  expect_false(is.na(res[2]))
})


test_that("divCoef validates abundance and distance inputs", {
  expect_error(divCoef(1:3), "matrix or data.frame")
  expect_error(divCoef(matrix(letters[1:4], 2)), "numeric")
  expect_error(divCoef(matrix(1, 3, 2), dis = diag(3)), "class 'dist'")
  expect_error(divCoef(matrix(1, 3, 2), dis = dist(matrix(1:4, 2))),
               "incompatible dimensions")
  x <- data.frame(a = c(1, 1, 1), b = c(1, 0, 0))
  expect_equal(divCoef(x), c(2 / 3, 0))
})

test_that("divCoef handles missing and zero columns independently", {
  x <- cbind(c(1, NA, 1), c(0, 0, 0), c(1, 1, 1))
  expect_equal(divCoef(x, na.rm = TRUE), c(NA_real_, 0, 2 / 3))
  expect_equal(divCoef(matrix(NA_real_, 3, 2), na.rm = TRUE), rep(NA_real_, 2))
})

test_that("divCoef matches a hand calculation for two entities", {
  d <- dist(matrix(c(0, 2), ncol = 1))
  x <- cbind(c(1, 3), c(1, 1), c(4, 0))
  # D = distance^2 * p1 * p2, with maximum distance^2 / 4 = 1.
  expect_equal(divCoef(x, dis = d), c(0.75, 1, 0))
  expect_equal(divCoef(x, dis = d, normalize = TRUE), c(0.75, 1, 0))
  expect_equal(divCoef(matrix(1, 4, 1), normalize = TRUE), 1)
})

test_that("divCoef warns for non-Euclidean distances and rejects a zero maximum", {
  d <- as.dist(matrix(c(0, 1, 1, 1, 0, 3, 1, 3, 0), 3))
  expect_warning(ans <- divCoef(matrix(1, 3, 1), d), "not Euclidean")
  expect_equal(ans, 11 / 9)
  expect_error(divCoef(matrix(1, 1, 2), normalize = TRUE), "maximum diversity is not positive")
})

test_that("divCoef maximization helper reports convergence and iteration exhaustion", {
  maximum <- DescToolsX:::.divCoefMax
  z <- maximum(matrix(0, 3, 3))
  expect_equal(z$value, 0)
  expect_equal(z$weights, rep(1 / 3, 3))
  expect_true(z$converged)
  expect_equal(z$iterations, 1)
  d2 <- matrix(1, 4, 4)
  diag(d2) <- 0
  z <- maximum(d2)
  expect_equal(z$value, 3 / 4)
  expect_equal(z$weights, rep(1 / 4, 4))
  expect_true(z$converged)
  d2 <- as.matrix(dist(matrix(c(0, 1, 2), ncol = 1)))^2 / 2
  z <- maximum(d2, maxit = 1)
  expect_false(z$converged)
  expect_equal(z$iterations, 1)
  expect_equal(sum(z$weights), 1)
  expect_true(all(z$weights >= 0))
})


# normalize = TRUE divides by the maximum of the coefficient over the
# simplex. The former power iteration found the Perron vector instead.

test_that("the maximum is reached on the boundary of the simplex", {
  # points 0, 1, 2: the maximum 1 lies at weights (1/2, 0, 1/2); the power
  # iteration stopped at 0.8165 and normalized this very column to 1.2247
  d <- dist(matrix(c(0, 1, 2), ncol = 1))
  expect_equal(.divCoefMax(as.matrix(d)^2 / 2)$value, 1, tolerance = 1e-7)
  expect_equal(divCoef(matrix(c(1, 0, 1), ncol = 1), d, normalize = TRUE),
               1, tolerance = 1e-7)
})

test_that("normalized values stay in [0, 1] and the maximum is attained", {
  set.seed(51)
  for (i in 1:30) {
    n <- sample(3:8, 1)
    d <- dist(matrix(rnorm(2 * n), ncol = 2))
    d2 <- as.matrix(d)^2 / 2
    mx <- .divCoefMax(d2)
    expect_true(mx$converged)
    # no random abundance vector beats the maximum
    p <- matrix(rexp(n * 200), nrow = n)
    p <- sweep(p, 2, colSums(p), "/")
    expect_true(all(colSums(p * (d2 %*% p)) <= mx$value * (1 + 1e-7)))
    # and its own weights attain it, i.e. the normalized value is 1
    w <- matrix(mx$weights, ncol = 1)
    expect_equal(divCoef(w, d, normalize = TRUE), 1, tolerance = 1e-7)
  }
})

test_that("without a distance matrix the maximum is 1 - 1/n", {
  x <- matrix(c(1, 1, 1, 1, 5, 1, 1, 1), ncol = 2)
  expect_equal(divCoef(x, normalize = TRUE), divCoef(x) / (1 - 1 / 4))
  expect_equal(divCoef(x, normalize = TRUE)[1], 1)
})

test_that("a degenerate distance matrix is refused", {
  # two identical points: every distance is zero
  expect_error(divCoef(matrix(c(1, 1), ncol = 1), dist(matrix(c(0, 0), ncol = 1)),
                       normalize = TRUE), "not positive")
})
