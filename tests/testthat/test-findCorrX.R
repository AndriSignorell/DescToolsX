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

test_that("findCorrX returns integer indices by default", {
  cmat <- .high_cormat()
  res  <- findCorrX(cmat, cutoff = 0.9)
  expect_type(res, "integer")
})

test_that("findCorrX return = 'names' returns column names", {
  cmat <- .high_cormat()
  res  <- findCorrX(cmat, cutoff = 0.9, return = "names")
  expect_type(res, "character")
  expect_true(all(res %in% colnames(cmat)))
})

test_that("findCorrX return = 'logical' has length ncol(x)", {
  cmat <- .high_cormat()
  res  <- findCorrX(cmat, cutoff = 0.9, return = "logical")
  expect_type(res, "logical")
  expect_length(res, ncol(cmat))
})

test_that("findCorrX return = 'report' has removed, kept, and log", {
  cmat <- .high_cormat()
  res  <- findCorrX(cmat, cutoff = 0.9, return = "report")
  expect_type(res, "list")
  expect_named(res, c("removed","kept","log"))
})

test_that("findCorrX identifies at least one variable when high correlation present", {
  cmat <- .high_cormat()
  res  <- findCorrX(cmat, cutoff = 0.9)
  expect_gte(length(res), 1L)
})

test_that("findCorrX returns empty integer(0) when no pair exceeds cutoff", {
  cmat <- .make_cormat()
  res  <- findCorrX(cmat, cutoff = 0.9999)
  expect_length(res, 0L)
})

test_that("findCorrX methods mean / max / median all work", {
  cmat <- .high_cormat()
  for (m in c("mean", "max", "median")) {
    res <- findCorrX(cmat, cutoff = 0.9, method = m)
    expect_type(res, "integer")
  }
})

test_that("findCorrX stops for non-symmetric matrix", {
  m <- matrix(1:9, 3, 3)
  expect_error(findCorrX(m, cutoff = 0.8), "symmetric")
})

test_that("findCorrX stops for non-matrix input", {
  expect_error(findCorrX(data.frame(a = 1:3), cutoff = 0.8), "matrix")
})

test_that("findCorrX stops when cutoff is out of (0, 1)", {
  cmat <- .make_cormat()
  expect_error(findCorrX(cmat, cutoff = 1.5))
  expect_error(findCorrX(cmat, cutoff = 0))
})

test_that("findCorrX removed + kept indices cover all original columns", {
  cmat <- .high_cormat()
  res  <- findCorrX(cmat, cutoff = 0.9, return = "report")
  all_idx <- sort(c(res$removed, res$kept))
  expect_equal(all_idx, seq_len(ncol(cmat)))
})
