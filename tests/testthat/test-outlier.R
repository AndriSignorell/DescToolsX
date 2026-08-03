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



test_that("outlier agrees with boxplot, as documented", {
  
  # boxplot.stats() builds its fences from the HINGES, not from the
  # type-7 quartiles. For 1:20 the hinges are 5.5/15.5 and the quartiles
  # 5.75/15.25, so the quantile fences were the narrower pair and this
  # function flagged points the boxplot did not.
  for (n in c(6, 7, 8, 10, 12, 20, 47)) {
    x <- c(seq_len(n), 10 * n)
    expect_equal(sort(outlier(x)), sort(boxplot.stats(x)$out),
                 label = paste("n =", n))
  }
  
  set.seed(1)
  z <- c(rnorm(50), 12, -9)
  expect_equal(sort(outlier(z)), sort(boxplot.stats(z)$out))
})


test_that("outlier returns values or indices consistently", {
  
  x <- c(1, 2, 3, 4, 5, 100)
  
  expect_equal(outlier(x), 100)
  expect_equal(outlier(x, value = FALSE), 6L)
  expect_equal(x[outlier(x, value = FALSE)], outlier(x))
  
  # NAs are dropped from the result, not reported as outliers
  y <- c(x, NA)
  expect_equal(outlier(y, na.rm = TRUE), 100)
  expect_false(anyNA(outlier(y, na.rm = TRUE)))
})

