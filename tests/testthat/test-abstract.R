test_that("abstract returns an object of class 'Abstract'", {
  res <- abstract(iris)
  expect_s3_class(res, "Abstract")
})

test_that("abstract returns one row per column of the data frame", {
  res <- abstract(iris)
  expect_equal(nrow(res), ncol(iris))
})

test_that("abstract has the expected column names", {
  res <- abstract(iris)
  expect_named(res, c("Nr","Class","ColName","NAs","Levels","Label"))
})

test_that("abstract Nr column runs from 1 to ncol(x)", {
  res <- abstract(iris)
  expect_equal(res$Nr, seq_len(ncol(iris)))
})

test_that("abstract ColName column matches names of input data frame", {
  res <- abstract(iris)
  expect_equal(res$ColName, names(iris))
})

test_that("abstract NAs column shows '.' when there are no NAs", {
  res <- abstract(iris)
  # iris has no NAs → zeroForm '.' in all rows
  expect_true(all(res$NAs == "."))
})

test_that("abstract NAs column shows count when NAs are present", {
  df <- data.frame(x = c(1, NA, 3, 4, 5))
  res <- abstract(df)
  # Should contain "1" somewhere in the NAs field
  expect_match(res$NAs[1], "1")
})

test_that("abstract maxLevels limits the number of displayed factor levels", {
  df <- data.frame(f = factor(letters[1:8]))
  res5 <- abstract(df, maxLevels = 5)
  # Should contain '...' because 8 > 5
  expect_match(res5$Levels[1], "\\.\\.\\.")
})

test_that("abstract maxVars limits the number of rows returned", {
  res <- abstract(iris, maxVars = 3)
  expect_equal(nrow(res), 3)
})

test_that("abstract nrow attribute equals nrow of the input data frame", {
  res <- abstract(iris)
  expect_equal(attr(res, "nrow"), nrow(iris))
})

test_that("abstract ncol attribute equals ncol of the input data frame", {
  res <- abstract(iris)
  expect_equal(attr(res, "ncol"), ncol(iris))
})

test_that("abstract prints without error", {
  expect_output(print(abstract(iris)))
})
