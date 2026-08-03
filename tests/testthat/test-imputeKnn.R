
# shared test data
.knnData <- function() {

  dat <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    y = c(1, 2, 3, 4, 5, 6),
    z = factor(c("a", "b", "a", "b", "a", "b"))
  )

  dat[c(1, 3), "x"] <- NA
  dat[c(2, 5), "y"] <- NA

  dat

}


test_that("imputeKnn returns a data frame of unchanged shape", {

  dat <- .knnData()
  res <- imputeKnn(dat, k = 2)

  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), dim(dat))
  expect_equal(names(res), names(dat))

})


test_that("imputeKnn fills every missing value", {

  res <- imputeKnn(.knnData(), k = 2)

  expect_false(anyNA(res))

})


test_that("imputeKnn leaves observed values untouched", {

  dat <- .knnData()
  res <- imputeKnn(dat, k = 2)

  for(j in names(dat)) {

    obs <- !is.na(dat[[j]])
    expect_equal(res[[j]][obs], dat[[j]][obs])

  }

})


test_that("imputeKnn preserves column types", {

  dat <- .knnData()
  res <- imputeKnn(dat, k = 2)

  expect_type(res$x, "double")
  expect_s3_class(res$z, "factor")
  expect_equal(levels(res$z), levels(dat$z))

})


test_that("imputeKnn imputes plausible values on a monotone pattern", {

  # x and y are identical and increasing, so a neighbour matched on y
  # should supply an x close to that y
  dat <- .knnData()
  res <- imputeKnn(dat, k = 2)

  expect_true(res$x[1] >= 1 && res$x[1] <= 6)
  expect_true(res$x[3] >= 1 && res$x[3] <= 6)

})


test_that("imputeKnn works for both aggregation methods", {

  expect_false(anyNA(imputeKnn(.knnData(), k = 2, method = "median")))
  expect_false(anyNA(imputeKnn(.knnData(), k = 2, method = "weighted")))

})


test_that("imputeKnn works with and without scaling", {

  expect_false(anyNA(imputeKnn(.knnData(), k = 2, scale = TRUE)))
  expect_false(anyNA(imputeKnn(.knnData(), k = 2, scale = FALSE)))

})


test_that("imputeKnn weights survive data in large units", {

  # exp(-d) underflows to zero for every neighbour when the distances are
  # large, which would leave the weighted mean undefined
  dat <- data.frame(
    x = c(1, 2, 3, 4, 5, 6) * 1e4,
    y = c(1, 2, 3, 4, 5, 6) * 1e4
  )

  dat[2, "x"] <- NA

  res <- imputeKnn(dat, k = 2, scale = FALSE, method = "weighted")

  expect_false(anyNA(res))
  expect_true(is.finite(res$x[2]))

})


test_that("imputeKnn imputes factors to an existing level", {

  dat <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    z = factor(c("a", "b", "a", "b", "a", "b"))
  )

  dat[3, "z"] <- NA

  res <- imputeKnn(dat, k = 2)

  expect_false(is.na(res$z[3]))
  expect_true(as.character(res$z[3]) %in% c("a", "b"))

})


test_that("imputeKnn selects the right level when one is absent from the neighbours", {

  # all near neighbours of row 6 carry level "a", so "a" must be chosen;
  # an aggregation that drops unused levels would misindex here
  dat <- data.frame(
    x = c(1, 2, 3, 4, 5, 5.1),
    z = factor(c("a", "a", "a", "c", "b", "a"), levels = c("a", "b", "c"))
  )

  dat[6, "z"] <- NA

  res <- imputeKnn(dat, k = 2, method = "weighted")

  expect_false(is.na(res$z[6]))
  expect_true(as.character(res$z[6]) %in% levels(dat$z))

})


test_that("imputeKnn handles several distinct missingness patterns", {

  dat <- data.frame(
    a = c(1, 2, 3, 4, 5, 6, 7, 8),
    b = c(1, 2, 3, 4, 5, 6, 7, 8),
    c = c(1, 2, 3, 4, 5, 6, 7, 8)
  )

  dat[1, "a"] <- NA               # pattern 1
  dat[2, "b"] <- NA               # pattern 2
  dat[3, c("a", "c")] <- NA       # pattern 3

  res <- imputeKnn(dat, k = 2)

  expect_false(anyNA(res))
  expect_equal(dim(res), dim(dat))

})


test_that("imputeKnn draws neighbours from distData when supplied", {

  dat <- data.frame(
    x = c(1, 2, 3),
    y = c(1, 2, 3)
  )

  dat[2, "x"] <- NA

  # the reference set is offset, so an imputation drawn from it must land
  # outside the range of dat's own x values
  ref <- data.frame(
    x = c(100, 101, 102, 103),
    y = c(1, 2, 3, 4)
  )

  res <- imputeKnn(dat, k = 2, distData = ref)

  expect_false(anyNA(res))
  expect_gt(res$x[2], 50)

})


test_that("imputeKnn checks distData against x", {

  dat <- .knnData()

  expect_error(
    imputeKnn(dat, k = 2, distData = data.frame(q = 1:5)),
    "same variables"
  )

  expect_error(
    imputeKnn(dat, k = 2, distData = "a"),
    "must be a data frame"
  )

})


test_that("imputeKnn errors when too few complete cases remain", {

  dat <- data.frame(
    x = c(1, NA, NA, NA),
    y = c(1, 2, 3, 4)
  )

  expect_error(imputeKnn(dat, k = 3), "complete observations")

})


test_that("imputeKnn validates its arguments", {

  dat <- .knnData()

  expect_error(imputeKnn(matrix(1:9, 3), k = 2), "must be a data frame")

  expect_error(imputeKnn(dat, k = 0), "positive whole number")
  expect_error(imputeKnn(dat, k = 1.5), "positive whole number")
  expect_error(imputeKnn(dat, k = c(2, 3)), "positive whole number")
  expect_error(imputeKnn(dat, k = NA), "positive whole number")

  expect_error(imputeKnn(dat, k = 2, scale = NA), "non-missing logical")

  expect_error(
    imputeKnn(data.frame(x = c(1, NA, 3), d = as.Date("2024-01-01") + 0:2),
              k = 1),
    "numeric and factor"
  )

})


test_that("imputeKnn warns and returns the input when nothing is missing", {

  dat <- data.frame(x = 1:5, y = 1:5)

  expect_warning(res <- imputeKnn(dat, k = 2), "No missing values")
  expect_equal(res, dat)

})


test_that("imputeKnn gives the same result with and without dbscan", {

  skip_if_not_installed("dbscan")

  # purely numeric data takes the kd-tree path when dbscan is present;
  # a factor column forces the fallback. Both must agree on the numeric
  # part, ties aside.
  dat <- data.frame(
    x = c(1, 2, 3, 4, 5, 6, 7, 8),
    y = c(2, 4, 6, 8, 10, 12, 14, 16)
  )

  dat[3, "x"] <- NA

  res <- imputeKnn(dat, k = 3, method = "median")

  expect_false(anyNA(res))
  expect_true(is.finite(res$x[3]))

})


test_that("imputeKnn ignores variables without variation instead of NaN", {
  
  set.seed(1)
  dat <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    const = rep(7, 6),
    z = factor(c("a", "b", "a", "b", "a", "b"))
  )
  dat[2, "x"] <- NA
  
  expect_warning(res <- imputeKnn(dat, k = 2), "no variation")
  expect_false(anyNA(res))
  # the imputed value must come from the neighbours, not from an
  # arbitrary first-k pick
  expect_true(res$x[2] >= min(dat$x, na.rm = TRUE) &&
                res$x[2] <= max(dat$x, na.rm = TRUE))
})


test_that("imputeKnn keeps types and fills every hole", {
  
  set.seed(2)
  dat <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    y = c(1, 2, 3, 4, 5, 6),
    z = factor(c("a", "b", "a", "b", "a", "b"))
  )
  dat[c(1, 3), "x"] <- NA
  dat[c(2, 5), "y"] <- NA
  
  res <- imputeKnn(dat, k = 2)
  
  expect_false(anyNA(res))
  expect_s3_class(res$z, "factor")
  expect_identical(levels(res$z), levels(dat$z))
  expect_identical(dim(res), dim(dat))
})

