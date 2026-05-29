test_that("scaleX returns a matrix", {
  x <- rnorm(20)
  expect_true(is.matrix(scaleX(x)))
})

test_that("scaleX centered result has median near 0", {
  set.seed(1)
  x <- rnorm(200, mean = 5, sd = 2)
  res <- scaleX(x)
  expect_lt(abs(median(res)), 0.1)
})

test_that("scaleX scaled result has mad near 1", {
  set.seed(2)
  x <- rnorm(200, mean = 5, sd = 2)
  res <- scaleX(x)
  expect_equal(mad(res), 1, tolerance = 0.05)
})

test_that("scaleX center = FALSE skips centering", {
  x <- matrix(c(1,2,3,4,5,6), ncol=2)
  res <- scaleX(x, center = FALSE, scale = FALSE)
  expect_equal(res[,1], x[,1])
})

test_that("scaleX scale = FALSE skips scaling", {
  x <- matrix(c(1,2,3,10,20,30), ncol=2)
  res <- scaleX(x, scale = FALSE)
  # column should have same spread but shifted to median = 0
  expect_equal(median(res[,1]), 0, tolerance = 1e-10)
  expect_equal(sd(res[,1]), sd(x[,1]), tolerance = 1e-10)
})

test_that("scaleX result has 'scaled:center' attribute", {
  x <- rnorm(30, mean=5)
  res <- scaleX(x)
  expect_false(is.null(attr(res, "scaled:center")))
})

test_that("scaleX result has 'scaled:scale' attribute", {
  x <- rnorm(30, sd=3)
  res <- scaleX(x)
  expect_false(is.null(attr(res, "scaled:scale")))
})

test_that("scaleX works column-wise for matrices", {
  x <- matrix(c(rnorm(20, mean=0), rnorm(20, mean=10)), ncol=2)
  res <- scaleX(x)
  expect_lt(abs(median(res[,1])), 0.2)
  expect_lt(abs(median(res[,2])), 0.2)
})
