
library(testthat)

## --- basic correctness ---

test_that("returns matrix with correct dimension and names", {
  set.seed(1)
  X <- matrix(rnorm(100 * 5), ncol = 5)
  colnames(X) <- paste0("V", 1:5)
  
  res <- corPart(X, x = 1:2, y = 3:4)
  
  expect_true(is.matrix(res))
  expect_equal(dim(res), c(2, 2))
  expect_equal(rownames(res), c("V1", "V2"))
  expect_equal(colnames(res), c("V1", "V2"))
})

---
  
  ## --- symmetry and correlation structure ---
  
  test_that("result is symmetric with unit diagonal", {
    set.seed(2)
    X <- matrix(rnorm(200), ncol = 5)
    
    res <- corPart(X, x = 1:3, y = 4:5)
    
    expect_equal(res, t(res), tolerance = 1e-10)
    expect_true(all(abs(diag(res) - 1) < 1e-10))
  })

---
  
  ## --- compare with known result (simple case) ---
  
  test_that("matches manual partial correlation (2 variables)", {
    set.seed(3)
    X <- matrix(rnorm(300), ncol = 3)
    
    # partial correlation of X1 and X2 controlling for X3
    res <- corPart(X, x = 1:2, y = 3)
    
    # manual formula
    C <- cor(X)
    r12 <- C[1,2]
    r13 <- C[1,3]
    r23 <- C[2,3]
    
    pc <- (r12 - r13*r23) / sqrt((1 - r13^2)*(1 - r23^2))
    
    expect_equal(res[1,2], pc, tolerance = 1e-6)
  })

---
  
  ## --- works with correlation matrix input ---
  
  test_that("works with correlation matrix input", {
    set.seed(4)
    X <- matrix(rnorm(100 * 5), ncol = 5)
    C <- cor(X)
    
    res1 <- corPart(X, x = 1:2, y = 3:4)
    res2 <- corPart(C, x = 1:2, y = 3:4)
    
    expect_equal(res1, res2, tolerance = 1e-10)
  })

---
  
  ## --- index validation ---
  
  test_that("invalid indices throw error", {
    X <- matrix(rnorm(50), ncol = 5)
    
    expect_error(corPart(X, x = c(0,1), y = 3))
    expect_error(corPart(X, x = c(1.5,2), y = 3))
    expect_error(corPart(X, x = 1:2, y = 6))
  })

---
  
  ## --- singular phi (collinearity) ---
  
  test_that("errors on singular phi matrix", {
    X <- matrix(rnorm(100), ncol = 3)
    
    # make collinearity in y
    X[,3] <- X[,2]
    
    expect_error(
      corPart(X, x = 1:2, y = 3),
      "singular|collinearity"
    )
  })

---
  
  ## --- non positive definite residual matrix ---
  
  test_that("errors when residual covariance is not positive definite", {
    # construct pathological case
    C <- matrix(c(
      1, 0.9, 0.9,
      0.9, 1, 0.9,
      0.9, 0.9, 1
    ), 3, 3)
    
    expect_error(
      corPart(C, x = 1:2, y = 3),
      "not positive definite"
    )
  })

---
  
  ## --- NA handling ---
  
  test_that("handles missing data via pairwise correlation", {
    set.seed(5)
    X <- matrix(rnorm(100), ncol = 4)
    X[sample(length(X), 10)] <- NA
    
    res <- corPart(X, x = 1:2, y = 3:4)
    
    expect_true(is.matrix(res))
  })

---
  
  ## --- invariance to ordering of x and y ---
  
  test_that("ordering of x does not affect values", {
    set.seed(6)
    X <- matrix(rnorm(200), ncol = 5)
    
    res1 <- corPart(X, x = c(1,2), y = c(3,4))
    res2 <- corPart(X, x = c(2,1), y = c(3,4))
    
    expect_equal(res1[1,2], res2[2,1], tolerance = 1e-10)
  })

---
  
  ## --- edge case: single variable ---
  
  test_that("works with single x variable", {
    set.seed(7)
    X <- matrix(rnorm(100), ncol = 4)
    
    res <- corPart(X, x = 1, y = 2:4)
    
    expect_equal(dim(res), c(1,1))
    expect_equal(res[1,1], 1)
  })

---
  
  # comparison with ppcor
  test_that("matches ppcor::pcor", {
    skip_if_not_installed("ppcor")
    
    library(ppcor)
    
    set.seed(8)
    X <- matrix(rnorm(200), ncol = 5)
    
    res <- corPart(X, x = 1:3, y = 4:5)
    pc <- pcor(X)$estimate[1:3, 1:3]
    
    expect_equal(res, pc, tolerance = 1e-5)
  })
