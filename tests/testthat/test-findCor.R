# ---- helper: build a correlation matrix with known high correlations ----
.make_cormat <- function(n = 5, seed = 42) {
  set.seed(seed)
  m <- matrix(rnorm(n * 100), ncol = n)
  colnames(m) <- paste0("V", seq_len(n))
  cor(m)
}

.high_cormat <- function() {
  # force V1 and V2 to be nearly identical (corr > 0.95)
  set.seed(1)
  base <- rnorm(100)
  m <- cbind(
    V1 = base,
    V2 = base + rnorm(100, sd = 0.05),
    V3 = rnorm(100),
    V4 = rnorm(100)
  )
  cor(m)
}

test_that("findCor returns integer indices by default", {
  cmat <- .high_cormat()
  res  <- findCor(cmat, cutoff = 0.9)
  expect_type(res, "integer")
})

test_that("findCor output = 'names' returns column names", {
  cmat <- .high_cormat()
  res  <- findCor(cmat, cutoff = 0.9, output = "names")
  expect_type(res, "character")
  expect_true(all(res %in% colnames(cmat)))
})

test_that("findCor output = 'logical' has length ncol(x)", {
  cmat <- .high_cormat()
  res  <- findCor(cmat, cutoff = 0.9, output = "logical")
  expect_type(res, "logical")
  expect_length(res, ncol(cmat))
})

test_that("findCor output = 'report' has removed, kept, and log", {
  cmat <- .high_cormat()
  res  <- findCor(cmat, cutoff = 0.9, output = "report")
  expect_type(res, "list")
  expect_named(res, c("removed","kept","log"))
})

test_that("findCor identifies at least one variable when high correlation present", {
  cmat <- .high_cormat()
  res  <- findCor(cmat, cutoff = 0.9)
  expect_gte(length(res), 1L)
})

test_that("findCor returns empty integer(0) when no pair exceeds cutoff", {
  cmat <- .make_cormat()
  res  <- findCor(cmat, cutoff = 0.9999)
  expect_length(res, 0L)
})

test_that("findCor methods mean / max / median all work", {
  cmat <- .high_cormat()
  for (m in c("mean", "max", "median")) {
    res <- findCor(cmat, cutoff = 0.9, method = m)
    expect_type(res, "integer")
  }
})

test_that("findCor stops for non-symmetric matrix", {
  m <- matrix(1:9, 3, 3)
  expect_error(findCor(m, cutoff = 0.8), "symmetric")
})

test_that("findCor stops for non-matrix input", {
  expect_error(findCor(data.frame(a = 1:3), cutoff = 0.8), "matrix")
})

test_that("findCor stops when cutoff is out of (0, 1)", {
  cmat <- .make_cormat()
  expect_error(findCor(cmat, cutoff = 1.5))
  expect_error(findCor(cmat, cutoff = 0))
})

test_that("findCor removed + kept indices cover all original columns", {
  cmat <- .high_cormat()
  res  <- findCor(cmat, cutoff = 0.9, output = "report")
  all_idx <- sort(c(res$removed, res$kept))
  expect_equal(all_idx, seq_len(ncol(cmat)))
})


test_that("findCor removes the higher-scoring variable of a pair", {
  
  cmat <- matrix(c(1,   0.95, 0.10,
                   0.95, 1,   0.12,
                   0.10, 0.12, 1), nrow = 3,
                 dimnames = list(paste0("V", 1:3), paste0("V", 1:3)))
  
  idx <- findCor(cmat, cutoff = 0.8)
  expect_length(idx, 1L)
  expect_true(idx %in% c(1L, 2L))
  
  # differing row and column names must not be read as asymmetry
  cm2 <- cmat
  rownames(cm2) <- paste0("r", 1:3)
  expect_silent(findCor(cm2, cutoff = 0.8))
  
  expect_error(findCor(unname(cmat), cutoff = 0.8, output = "names"),
               "output = 'index'")
})


# Review 25.09.2026 ------------------------------------------------------------

test_that("verbose reports each removal", {
  cm <- matrix(c(1, 0.95, 0.2, 0.95, 1, 0.3, 0.2, 0.3, 1), 3,
               dimnames = list(letters[1:3], letters[1:3]))
  expect_message(findCor(cm, cutoff = 0.9, verbose = TRUE), "Comparing")
  expect_silent(findCor(cm, cutoff = 0.9))
})

test_that("findCor validates type, cutoff and verbose", {
  cm <- diag(3)
  expect_error(findCor(matrix(letters[1:4], 2), cutoff = 0.8), "numeric")
  expect_error(findCor(cm, cutoff = NA_real_), "cutoff")
  expect_error(findCor(cm, cutoff = c(0.5, 0.6)), "cutoff")
  expect_error(findCor(cm, verbose = NA), "verbose")
  expect_error(findCor(matrix(1), cutoff = 0.5), "two variables")
})
