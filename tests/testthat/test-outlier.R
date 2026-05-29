test_that("outlier (boxplot) returns known outlier", {
  x <- c(rep(5, 20), 100)
  expect_true(100 %in% outlier(x))
})

test_that("outlier (boxplot) returns values, not indices by default", {
  x <- c(rep(5, 10), 100)
  res <- outlier(x)
  expect_true(all(res %in% x))
})

test_that("outlier value = FALSE returns indices", {
  x <- c(rep(5, 10), 100)
  idx <- outlier(x, value = FALSE)
  expect_type(idx, "integer")
  expect_true(all(idx <= length(x)))
})

test_that("outlier matches boxplot$out", {
  set.seed(1)
  x <- c(rnorm(50), 10, -10)
  expect_equal(sort(outlier(x)), sort(boxplot(x, plot=FALSE)$out))
})

test_that("outlier hampel method returns a numeric vector", {
  x <- c(rnorm(50), 20)
  res <- outlier(x, method = "hampel")
  expect_true(is.numeric(res))
})

test_that("outlier hampel detects extreme value", {
  x <- c(rep(0, 50), 1000)
  res <- outlier(x, method = "hampel")
  expect_true(1000 %in% res)
})

test_that("outlier na.rm = TRUE handles NAs", {
  x <- c(rep(5, 10), 100, NA)
  res <- outlier(x, na.rm = TRUE)
  expect_false(any(is.na(res)))
})

test_that("outlier returns empty vector when no outliers present", {
  x <- c(1, 2, 3, 4, 5)
  res <- outlier(x)
  expect_length(res, 0)
})
