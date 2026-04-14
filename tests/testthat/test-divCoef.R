

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


# test_that("divCoef matches DescTools implementation", {
#   set.seed(42)
#   
#   df <- matrix(runif(50), ncol = 5)
#   d <- dist(matrix(rnorm(20), ncol = 2))
#   
#   res1 <- divCoef(df, d)
#   res2 <- DescTools::divCoef(df, d)
#   
#   expect_equal(res1, res2, tolerance = 1e-6)
# })



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