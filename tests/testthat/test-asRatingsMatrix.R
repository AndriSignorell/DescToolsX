test_that("numeric data frames and lists become numeric matrices", {
  df <- data.frame(r1 = c(1, 2, 3), r2 = c(1, 3, 3), r3 = c(2, 2, 3))
  m <- .asRatingsMatrix(df)
  expect_true(is.numeric(m))
  expect_identical(dim(m), c(3L, 3L))
  expect_identical(colnames(m), c("r1", "r2", "r3"))
  expect_equal(unname(m[, "r2"]), c(1, 3, 3))
  expect_identical(.asRatingsMatrix(as.list(df)), m)
})

test_that("mixed columns are converted column-wise, without padding", {
  df <- data.frame(r1 = c(1, 10), r2 = c("1", "10"))
  # the trap: as.matrix() pads the numeric column to a common width
  expect_identical(unname(as.matrix(df)[1, "r1"]), " 1")
  m <- .asRatingsMatrix(df)
  expect_true(is.character(m))
  expect_identical(m[, "r1"], m[, "r2"])
})

test_that("factor columns contribute their labels", {
  df <- data.frame(r1 = factor(c("lo", "hi")), r2 = factor(c("hi", "hi")))
  m <- .asRatingsMatrix(df)
  expect_identical(m[, "r1"], c("lo", "hi"))
  expect_identical(m[, "r2"], c("hi", "hi"))
})

test_that("a matrix is passed through unchanged", {
  m <- matrix(c(1, 2, 2, 1, 3, 3), ncol = 2)
  expect_identical(.asRatingsMatrix(m), m)
})

test_that("malformed ratings are rejected", {
  expect_error(.asRatingsMatrix(list()), "at least two raters")
  expect_error(.asRatingsMatrix(data.frame(r1 = 1:3)), "at least two raters")
  expect_error(.asRatingsMatrix(matrix(1:3, ncol = 1)), "at least two raters")
  expect_error(.asRatingsMatrix(list(1:3, 1:2)), "equal length")
  expect_error(.asRatingsMatrix(1:6), "subjects in rows")
  expect_error(.asRatingsMatrix(array(1:8, c(2, 2, 2))), "subjects in rows")
})
