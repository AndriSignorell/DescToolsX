# tests/testthat/test-percAgreement.R

library(testthat)

# --------------------------------------------------
# Basic functionality
# --------------------------------------------------

test_that("works with simple confusion matrix", {
  x <- matrix(c(10, 2,
                3, 15), nrow = 2)
  
  res <- percAgreement(x)
  
  expect_true(is.numeric(res))
  expect_named(res, c("est", "lci", "uci"))
  expect_true(res["est"] >= 0 && res["est"] <= 1)
})

# --------------------------------------------------
# Perfect agreement
# --------------------------------------------------

test_that("returns 1 for perfect agreement", {
  
  x <- diag(10)
  rownames(x) <- colnames(x) <- LETTERS[1:10]
  res <- percAgreement(x)
  expect_equal(res[["est"]], 1)
  
})

# --------------------------------------------------
# No agreement
# --------------------------------------------------

test_that("returns near 0 for no agreement", {
  x <- matrix(c(0, 10,
                10, 0), nrow = 2)
  
  res <- percAgreement(x)
  
  expect_equal(res[["est"]], 0)
})

# --------------------------------------------------
# Ratings matrix (multiple raters)
# --------------------------------------------------

test_that("works with rating matrix", {
  x <- data.frame(
    r1 = c(1, 1, 2, 2),
    r2 = c(1, 2, 2, 2),
    r3 = c(1, 1, 2, NA)
  )
  
  res <- percAgreement(x)
  
  expect_true(is.numeric(res))
  expect_true(res["est"] >= 0 && res["est"] <= 1)
})

# --------------------------------------------------
# Handling of NA rows
# --------------------------------------------------

test_that("handles rows with insufficient ratings", {
  x <- data.frame(
    r1 = c(1, NA, 2),
    r2 = c(1, NA, NA),
    r3 = c(1, NA, 2)
  )
  
  res <- percAgreement(x, verbose = TRUE)
  
  expect_true(res$nPairable < res$n)
})

# --------------------------------------------------
# Confidence interval bounds
# --------------------------------------------------

test_that("CI is within [0,1]", {
  x <- matrix(c(5, 5,
                5, 5), nrow = 2)
  
  res <- percAgreement(x)
  
  expect_true(res["lci"] >= 0)
  expect_true(res["uci"] <= 1)
})

# --------------------------------------------------
# Verbose output structure
# --------------------------------------------------

test_that("verbose output returns full list", {
  x <- matrix(c(10, 0,
                0, 10), nrow = 2)
  
  res <- percAgreement(x, verbose = TRUE)
  
  expect_true(is.list(res))
  expect_named(res, c("estimate", "se", "conf.int", "n", "nPairable", "method"))
})

# --------------------------------------------------
# Input via x + y
# --------------------------------------------------

test_that("x and y inputs produce same result as confusion matrix", {
  x <- c(1,1,2,2)
  y <- c(1,2,2,2)
  
  tab <- table(x, y)
  
  res1 <- percAgreement(x, y)
  res2 <- percAgreement(tab)
  
  expect_equal(res1["est"], res2["est"])
})

# --------------------------------------------------
# Edge case: single observation
# --------------------------------------------------

test_that("handles n = 1 gracefully", {
  x <- matrix(1, nrow = 1)
  
  res <- percAgreement(x, verbose = TRUE)
  
  expect_true(is.na(res$se))
})

# --------------------------------------------------
# FPC effect
# --------------------------------------------------

test_that("fpc reduces variance", {
  x <- matrix(c(10, 2,
                3, 15), nrow = 2)
  
  res1 <- percAgreement(x, verbose = TRUE, fpc = 0)
  res2 <- percAgreement(x, verbose = TRUE, fpc = 0.5)
  
  expect_true(res2$se <= res1$se)
})

# --------------------------------------------------
# Invalid input
# --------------------------------------------------

test_that("fails with invalid input", {
  expect_error(percAgreement("not valid"))
})
