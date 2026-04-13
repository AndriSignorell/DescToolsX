

library(testthat)

test_that("DivCoef works for basic input", {
  set.seed(1)
  df <- matrix(runif(20), ncol = 4)
  d <- dist(matrix(rnorm(10), ncol = 2))
  
  res <- DivCoef(df, d)
  
  expect_type(res, "double")
  expect_length(res, ncol(df))
  expect_true(all(res >= 0))
})

test_that("DivCoef handles zero columns correctly", {
  df <- matrix(0, nrow = 5, ncol = 3)
  d <- dist(matrix(rnorm(10), ncol = 2))
  
  res <- DivCoef(df, d)
  
  expect_true(all(res == 0))
})

test_that("DivCoef errors on negative values", {
  df <- matrix(c(1, -1, 2, 3), ncol = 2)
  
  expect_error(DivCoef(df))
})

test_that("DivCoef default distance works", {
  df <- matrix(runif(20), ncol = 4)
  
  res <- DivCoef(df)
  
  expect_length(res, ncol(df))
})

test_that("Normalization scales values <= 1", {
  set.seed(1)
  df <- matrix(runif(20), ncol = 4)
  d <- dist(matrix(rnorm(10), ncol = 2))
  
  res <- DivCoef(df, d, normalize = TRUE)
  
  expect_true(all(res <= 1 + 1e-8))
})


# test_that("DivCoef matches DescTools implementation", {
#   set.seed(42)
#   
#   df <- matrix(runif(50), ncol = 5)
#   d <- dist(matrix(rnorm(20), ncol = 2))
#   
#   res1 <- DivCoef(df, d)
#   res2 <- DescTools::DivCoef(df, d)
#   
#   expect_equal(res1, res2, tolerance = 1e-6)
# })



test_that("Scaling df does not change result", {
  set.seed(1)
  df <- matrix(runif(20), ncol = 4)
  d <- dist(matrix(rnorm(10), ncol = 2))
  
  res1 <- DivCoef(df, d)
  res2 <- DivCoef(2 * df, d)
  
  expect_equal(res1, res2, tolerance = 1e-8)
})


test_that("Row permutation does not change result", {
  set.seed(1)
  df <- matrix(runif(20), ncol = 4)
  d <- dist(matrix(rnorm(10), ncol = 2))
  
  perm <- sample(nrow(df))
  
  res1 <- DivCoef(df, d)
  res2 <- DivCoef(df[perm, ], as.dist(as.matrix(d)[perm, perm]))
  
  expect_equal(res1, res2, tolerance = 1e-8)
})