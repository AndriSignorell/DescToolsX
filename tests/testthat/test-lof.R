

test_that("lof returns a numeric vector of length nrow(data)", {
  set.seed(1)
  data <- matrix(rnorm(40), ncol = 2)
  res  <- lof(data, k = 3)
  expect_type(res, "double")
  expect_length(res, nrow(data))
})

test_that("lof scores are positive", {
  set.seed(2)
  data <- matrix(rnorm(60), ncol = 2)
  res  <- lof(data, k = 5)
  expect_true(all(res > 0))
})

test_that("lof identifies an obvious outlier with a high score", {
  # Cluster of points near origin + one extreme outlier
  set.seed(3)
  data   <- rbind(matrix(rnorm(40, sd = 0.1), ncol = 2),
                  matrix(c(100, 100), ncol = 2))
  scores <- lof(data, k = 5)
  outlier_idx <- nrow(data)
  expect_gt(scores[outlier_idx], max(scores[-outlier_idx]))
})

test_that("lof scores for a uniform cluster are near 1 on average", {
  set.seed(4)
  data <- matrix(rnorm(100, sd = 0.01), ncol = 2)
  res  <- lof(data, k = 5)
  # Interior points in a uniform cluster have LOF ≈ 1; a few boundary points
  # may deviate significantly, so check the median rather than all values
  expect_lt(median(abs(res - 1)), 0.3)
})

test_that("lof works with a data frame input", {
  set.seed(5)
  df  <- data.frame(x = rnorm(30), y = rnorm(30))
  res <- lof(df, k = 3)
  expect_length(res, 30)
})

