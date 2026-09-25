
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

# ---
  ## --- symmetry and correlation structure ---
  
  test_that("result is symmetric with unit diagonal", {
    set.seed(2)
    X <- matrix(rnorm(200), ncol = 5)
    
    res <- corPart(X, x = 1:3, y = 4:5)
    
    expect_equal(res, t(res), tolerance = 1e-10)
    expect_true(all(abs(diag(res) - 1) < 1e-10))
  })

# ---
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

# ---
  ## --- works with correlation matrix input ---
  
  test_that("works with correlation matrix input", {
    set.seed(4)
    X <- matrix(rnorm(100 * 5), ncol = 5)
    C <- cor(X)
    
    res1 <- corPart(X, x = 1:2, y = 3:4)
    res2 <- corPart(C, x = 1:2, y = 3:4)
    
    expect_equal(res1, res2, tolerance = 1e-10)
  })

# ---
  ## --- index validation ---
  
  test_that("invalid indices throw error", {
    X <- matrix(rnorm(50), ncol = 5)
    
    expect_error(corPart(X, x = c(0,1), y = 3))
    expect_error(corPart(X, x = c(1.5,2), y = 3))
    expect_error(corPart(X, x = 1:2, y = 6))
  })

# ---
  ## --- singular phi (collinearity) ---
  
  test_that("errors on singular phi matrix", {
    X <- matrix(rnorm(100), ncol = 4)
    
    # perfekte Kollinearität in y
    X[,4] <- X[,3]
    
    expect_error(
      corPart(X, x = 1:2, y = 3:4),
      "singular|collinearity"
    )
  })

# ---
  ## --- non positive definite residual matrix ---
  
  test_that("errors when covariance matrix is singular", {
    X <- matrix(rnorm(99), ncol = 3)
    X[,3] <- X[,2]  # perfekte Kollinearität
    
    expect_error(
      corPart(X, x = 1:2, y = 3),
      "singular|collinearity"
    )
  })

# ---
  ## --- NA handling ---
  
  test_that("handles missing data via pairwise correlation", {
    set.seed(5)
    X <- matrix(rnorm(100), ncol = 4)
    X[sample(length(X), 10)] <- NA
    
    res <- corPart(X, x = 1:2, y = 3:4)
    
    expect_true(is.matrix(res))
  })

# ---
  ## --- invariance to ordering of x and y ---
  
  test_that("ordering of x does not affect values", {
    set.seed(6)
    X <- matrix(rnorm(200), ncol = 5)
    
    res1 <- corPart(X, x = c(1,2), y = c(3,4))
    res2 <- corPart(X, x = c(2,1), y = c(3,4))
    
    expect_equal(res1[1,2], res2[2,1], tolerance = 1e-10)
  })

# ---
  ## --- edge case: single variable ---
  
  test_that("works with single x variable", {
    set.seed(7)
    X <- matrix(rnorm(100), ncol = 4)
    
    res <- corPart(X, x = 1, y = 2:4)
    
    expect_equal(dim(res), c(1,1))
    expect_equal(res[1,1], 1)
  })

# ---
  # Compare each pair while controlling only for the specified y variables.
  test_that("corPart matches pairwise ppcor::pcor.test with the same controls", {
    skip_if_not_installed("ppcor")
    set.seed(8)
    X <- matrix(rnorm(200), ncol = 5)
    res <- corPart(X, x = 1:3, y = 4:5)
    for (i in 1:2) for (j in (i + 1):3) {
      ref <- ppcor::pcor.test(X[, i], X[, j], X[, 4:5])$estimate
      expect_equal(unname(res[i, j]), unname(ref), tolerance = 1e-10)
    }
  })



test_that("corPart works for a single variable of interest", {
  
  set.seed(1)
  X <- matrix(rnorm(100 * 4), ncol = 4)
  colnames(X) <- paste0("V", 1:4)
  
  # diag(v) with length-1 v used to build an identity matrix of size
  # round(v) instead of a 1x1 matrix
  pc <- corPart(cor(X), x = 1, y = 3:4)
  
  expect_equal(dim(pc), c(1L, 1L))
  expect_equal(unname(pc[1, 1]), 1)
})


test_that("corPart agrees with the Schur complement and with cor of residuals", {
  
  set.seed(2)
  X <- matrix(rnorm(200 * 4), ncol = 4)
  colnames(X) <- paste0("V", 1:4)
  
  pc <- corPart(X, x = 1:2, y = 3:4)
  
  r1 <- residuals(lm(X[, 1] ~ X[, 3] + X[, 4]))
  r2 <- residuals(lm(X[, 2] ~ X[, 3] + X[, 4]))
  
  expect_equal(unname(pc[1, 2]), unname(cor(r1, r2)), tolerance = 1e-10)
  expect_true(isSymmetric(unname(pc)))
})


test_that("a square data matrix is not mistaken for a covariance matrix", {
  
  set.seed(3)
  X <- matrix(rnorm(25), nrow = 5)   # 5 observations, 5 variables
  colnames(X) <- paste0("V", 1:5)
  
  expect_silent(pc <- corPart(X, x = 1:2, y = 3:4))
  expect_equal(dim(pc), c(2L, 2L))
})


# Additional coverage: explicit branches and reference results
test_that("corPart converts numeric data frames and refuses text", {
  X <- cbind(a = c(1, 0, 0, 2, 3), b = c(0, 1, 0, 3, 1),
             c = c(0, 0, 1, 1, 4))
  expect_equal(corPart(as.data.frame(X), 1:2, 3), corPart(X, 1:2, 3))
  expect_error(corPart(matrix(letters[1:9], 3), 1:2, 3), "must be numeric")
})

test_that("corPart validates both index vectors independently", {
  S <- diag(4)
  expect_error(corPart(S, integer(), 3), "at least one variable")
  expect_error(corPart(S, 1:2, integer()), "at least one variable")
  for (bad in list(NA_real_, NaN, Inf, -Inf, 0, -1, 1.5, 5)) {
    expect_error(corPart(S, bad, 3), "integer indices")
    expect_error(corPart(S, 1:2, bad), "integer indices")
  }
  expect_error(corPart(S, 1:2, 2:3), "must not overlap")
})

test_that("corPart diagnoses missing covariance and ill-conditioning", {
  X <- cbind(c(1, 2, 4, 8), c(3, 1, 5, 2), rep(NA_real_, 4))
  expect_error(corPart(X, 1:2, 3), "covariance matrix contains missing")
  # nearly collinear on the correlation scale: V2 and V3 correlate 1 - 1e-10
  S <- diag(3)
  S[2, 3] <- S[3, 2] <- 1 - 1e-10
  expect_error(corPart(S, 1:2, 3), "singular or ill-conditioned")
  # a merely tiny variance is not ill-conditioning (it used to be reported
  # as such, because rcond() ran on the covariance scale)
  expect_equal(unname(corPart(diag(c(1, 1, 1e-12)), 1:2, 3)), diag(2))
})

test_that("corPart handles covariance scale and mismatched dimnames", {
  S <- matrix(c(4, 1, 1, 1, 9, 2, 1, 2, 16), 3)
  expected <- (1 - 1 * 2 / 16) / sqrt((4 - 1 / 16) * (9 - 4 / 16))
  dimnames(S) <- list(c("r1", "r2", "r3"), c("a", "b", "c"))
  ans <- corPart(S, 1:2, 3)
  expect_equal(unname(ans[1, 2]), expected)
  expect_identical(dimnames(ans), list(c("a", "b"), c("a", "b")))
  expect_equal(unname(corPart(cov2cor(unname(S)), 1:2, 3)), unname(ans))
})


test_that("corPart controls only for y with three target variables", {
  S <- matrix(0.5, 4, 4)
  diag(S) <- 1
  expected <- matrix(1 / 3, 3, 3)
  diag(expected) <- 1
  expect_equal(corPart(S, 1:3, 4), expected, tolerance = 1e-12)
  # The former precision-matrix implementation returned 1/4 off-diagonal.
  expect_equal(corPart(S, 1:3, 4)[1:2, 1:2], corPart(S, 1:2, 4))
})

test_that("corPart matches regression residuals for multiple target variables", {
  set.seed(801)
  X <- matrix(rnorm(300 * 6), ncol = 6)
  X[, 1] <- X[, 1] + X[, 4] + 0.5 * X[, 5]
  X[, 2] <- X[, 2] + X[, 4] + X[, 6]
  X[, 3] <- X[, 3] - X[, 5] + X[, 6]
  colnames(X) <- paste0("V", 1:6)
  targets <- c(3, 1, 2)
  controls <- c(6, 4, 5)
  residuals <- vapply(targets, function(j) {
    stats::residuals(lm(X[, j] ~ X[, controls]))
  }, numeric(nrow(X)))
  expected <- cor(residuals)
  dimnames(expected) <- list(colnames(X)[targets], colnames(X)[targets])
  ans <- corPart(X, targets, controls)
  expect_equal(ans, expected, tolerance = 1e-10)
  expect_equal(corPart(cov(X), targets, controls), expected, tolerance = 1e-10)
  expect_equal(corPart(cor(X), targets, controls), expected, tolerance = 1e-10)
  expect_equal(corPart(X, targets, rev(controls)), ans, tolerance = 1e-10)
  expect_equal(corPart(X, targets[1:2], controls), ans[1:2, 1:2], tolerance = 1e-10)
})

test_that("corPart diagnoses non-finite covariance and invalid residual variances", {
  S <- diag(3)
  S[1, 1] <- Inf
  expect_error(corPart(S, 1:2, 3), "finite values")
  S <- diag(3)
  S[1, 3] <- S[3, 1] <- 2
  expect_error(corPart(S, 1:2, 3), "Residual variances")
})


# Review 25.09.2026: condition check on the correlation scale, index checks
test_that("corPart is not fooled by variables on very different scales", {
  set.seed(1)
  X <- matrix(rnorm(800), ncol = 4, dimnames = list(NULL, paste0("V", 1:4)))
  Xs <- X
  Xs[, 1] <- Xs[, 1] * 1e4          # uncorrelated, only the units differ
  Xs[, 4] <- Xs[, 4] * 1e-3
  expect_no_error(pc <- corPart(Xs, x = 1:2, y = 3:4))
  expect_equal(pc, corPart(X, x = 1:2, y = 3:4))
  r1 <- resid(lm(Xs[, 1] ~ Xs[, 3:4]))
  r2 <- resid(lm(Xs[, 2] ~ Xs[, 3:4]))
  expect_equal(unname(pc[1, 2]), cor(r1, r2))
})

test_that("raw data and its pairwise covariance matrix give the same result", {
  set.seed(9)
  X <- matrix(rnorm(800), ncol = 4)
  X[sample(length(X), 40)] <- NA
  expect_equal(corPart(X, x = 1:2, y = 3:4),
               corPart(cov(X, use = "pairwise.complete.obs"), x = 1:2, y = 3:4))
})

test_that("corPart rejects logical, duplicate and constant selections", {
  set.seed(10)
  X <- matrix(rnorm(200), ncol = 4)
  expect_error(corPart(X, x = TRUE, y = 2), "integer indices")
  expect_error(corPart(X, x = c(1, 1), y = 3), "duplicate")
  expect_error(corPart(X, x = 1:2, y = c(3, 3)), "duplicate")
  expect_error(corPart(cbind(X, 5), x = 1:2, y = c(3, 5)),
               "index 5 have no positive variance")
})
