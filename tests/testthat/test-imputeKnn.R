# shared test data
.knn_data <- function() {
  set.seed(123)
  dat <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    y = c(1, 2, 3, 4, 5, 6),
    z = factor(c("a","b","a","b","a","b"))
  )
  dat[c(1,3), "x"] <- NA
  dat[c(2,5), "y"] <- NA
  dat
}

test_that("imputeKnn returns a data frame", {
  expect_s3_class(imputeKnn(.knn_data(), k = 2), "data.frame")
})

test_that("imputeKnn returns same dimensions as input", {
  dat <- .knn_data()
  res <- imputeKnn(dat, k = 2)
  expect_equal(dim(res), dim(dat))
})

test_that("imputeKnn removes all NAs from numeric columns", {
  res <- imputeKnn(.knn_data(), k = 2)
  expect_false(any(is.na(res$x)))
  expect_false(any(is.na(res$y)))
})

test_that("imputeKnn non-missing values remain unchanged", {
  dat <- .knn_data()
  res <- imputeKnn(dat, k = 2)
  obs_x <- !is.na(dat$x)
  expect_equal(res$x[obs_x], dat$x[obs_x])
})

test_that("imputeKnn method = 'median' also removes all NAs", {
  res <- imputeKnn(.knn_data(), k = 2, method = "median")
  expect_false(any(is.na(res$x)))
})

test_that("imputeKnn scale = FALSE also works", {
  res <- imputeKnn(.knn_data(), k = 2, scale = FALSE)
  expect_false(any(is.na(res$x)))
})

test_that("imputeKnn stops for non-data.frame input", {
  expect_error(imputeKnn(matrix(1:9, 3), k = 2), "data.frame")
})

test_that("imputeKnn stops for invalid k", {
  expect_error(imputeKnn(.knn_data(), k = 0))
  expect_error(imputeKnn(.knn_data(), k = 1.5))
})

test_that("imputeKnn warns when no missing values present", {
  dat <- data.frame(x = 1:5, y = 1:5)
  expect_warning(imputeKnn(dat, k = 2), "missing")
})
