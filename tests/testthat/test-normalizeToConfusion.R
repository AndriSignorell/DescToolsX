
# normalizeToConfusion -- test suite
# run with: testthat::test_file("test-normalizeToConfusion.R")

library(testthat)

# ── helpers ───────────────────────────────────────────────────────────────────

A <- c("pos", "neg", "pos", "inc")
B <- c("pos", "pos", "neg", "inc")

x <- c(3, 3, 3, 4, 5, 5, 2)
y <- c(3, 6, 4, 6, 2, 4, 2)

ratingscale <- sort(unique(c(x, y)))

# ── 1. two vectors ────────────────────────────────────────────────────────────

test_that("two character vectors produce correct square table", {
  tab <- normalizeToConfusion(A, B)
  expect_true(is.matrix(tab))
  expect_equal(nrow(tab), ncol(tab))
  expect_equal(rownames(tab), colnames(tab))
  expect_equal(sum(tab), length(A))
})

test_that("two numeric vectors produce correct square table", {
  tab <- normalizeToConfusion(x, y)
  expect_true(is.matrix(tab))
  expect_equal(nrow(tab), ncol(tab))
  expect_equal(sum(tab), length(x))
})

test_that("levels argument controls factor levels for two vectors", {
  lvls <- c("neg", "pos", "inc")
  tab  <- normalizeToConfusion(A, B, levels = lvls)
  expect_equal(rownames(tab), lvls)
  expect_equal(colnames(tab), lvls)
})

test_that("useNA = 'always' adds NA row and column", {
  A2    <- c("pos", "neg", "pos", "inc")
  B2    <- c("pos", "pos", "neg", "inc")
  B2[2] <- NA
  tab   <- normalizeToConfusion(A2, B2, useNA = "always")
  print(colnames(tab))
  expect_true(any(is.na(colnames(tab))) || "<NA>" %in% colnames(tab) || "NA" %in% colnames(tab))
})

# ── 2. table input ────────────────────────────────────────────────────────────

test_that("table input is passed through correctly", {
  tab_in  <- table(factor(x, levels = ratingscale),
                   factor(y, levels = ratingscale))
  tab_out <- normalizeToConfusion(tab_in)
  expect_true(is.matrix(tab_out))
  expect_equal(dim(tab_out), dim(tab_in))
  expect_equal(as.integer(tab_out), as.integer(tab_in))
})

test_that("table input with levels renames dimnames", {
  tab_in  <- table(factor(x, levels = ratingscale),
                   factor(y, levels = ratingscale))
  new_lvls <- paste0("L", ratingscale)
  tab_out  <- normalizeToConfusion(tab_in, levels = new_lvls)
  expect_equal(rownames(tab_out), new_lvls)
  expect_equal(colnames(tab_out), new_lvls)
})

test_that("non-square table input raises error in agreement mode", {
  tab_in <- table(c(1,1,2), c(1,2,3))
  expect_error(normalizeToConfusion(tab_in))
})

test_that("table with mismatched dimnames raises error in agreement mode", {
  tab_in          <- table(factor(x, levels = ratingscale),
                           factor(y, levels = ratingscale))
  rownames(tab_in) <- paste0("r", rownames(tab_in))
  expect_error(normalizeToConfusion(tab_in), "row and column names must match")
})

# ── 3. matrix input ───────────────────────────────────────────────────────────

test_that("square named matrix is treated as confusion table", {
  m <- matrix(c(5,1,0,3), nrow = 2,
              dimnames = list(c("a","b"), c("a","b")))
  tab <- normalizeToConfusion(m)
  expect_equal(tab, m)
})

test_that("2-column non-square matrix is treated as rater matrix", {
  m   <- cbind(x, y)
  tab <- normalizeToConfusion(m)
  expect_true(is.matrix(tab))
  expect_equal(nrow(tab), ncol(tab))           # result is square
  expect_equal(sum(tab), length(x))
})

test_that("matrix with != 2 columns and not a confusion table raises error", {
  m <- matrix(1:12, nrow = 3)
  expect_error(normalizeToConfusion(m))
})

# ── 4. data.frame input ───────────────────────────────────────────────────────

test_that("data.frame with 2 columns is treated as rater matrix", {
  df  <- data.frame(x, y)
  tab <- normalizeToConfusion(df)
  expect_true(is.matrix(tab))
  expect_equal(sum(tab), length(x))
})

test_that("data.frame with != 2 columns raises error", {
  df <- data.frame(x, y, z = x)
  expect_error(normalizeToConfusion(df))
})

# ── 5. list input ─────────────────────────────────────────────────────────────

test_that("list with 2 elements is treated as rater pair", {
  tab <- normalizeToConfusion(list(A, B))
  expect_true(is.matrix(tab))
  expect_equal(sum(tab), length(A))
})

test_that("list with 2 numeric elements matches two-vector result", {
  tab_list <- normalizeToConfusion(list(x, y))
  tab_vec  <- normalizeToConfusion(x, y)
  expect_equal(tab_list, tab_vec)
})

test_that("list with != 2 elements raises error", {
  expect_error(normalizeToConfusion(list(A, B, A)))
})

# ── 6. mode = "association" ───────────────────────────────────────────────────

test_that("association mode allows rectangular table", {
  tab <- normalizeToConfusion(
    c("a","a","b"), c("x","y","x"),
    mode = "association"
  )
  expect_equal(dim(tab), c(2L, 2L))
})

test_that("association mode accepts list levels", {
  tab <- normalizeToConfusion(
    c("a","b","a"), c("x","y","x"),
    levels = list(c("a","b"), c("x","y","z")),
    mode   = "association"
  )
  expect_equal(ncol(tab), 3L)
  expect_equal(colnames(tab), c("x","y","z"))
})

test_that("association mode rejects atomic levels", {
  expect_error(
    normalizeToConfusion(
      c("a","b"), c("x","y"),
      levels = c("a","b"),
      mode   = "association"
    ),
    "list"
  )
})

# ── 7. edge cases ─────────────────────────────────────────────────────────────

test_that("single vector without y raises error", {
  expect_error(normalizeToConfusion(A))
})

test_that("unsupported input type raises error", {
  expect_error(normalizeToConfusion(42L))
})

test_that("result is always a numeric matrix", {
  tab <- normalizeToConfusion(A, B)
  expect_true(is.matrix(tab))
  expect_true(is.numeric(tab))
})

test_that("row and column sums are consistent with input length", {
  tab <- normalizeToConfusion(x, y)
  expect_equal(sum(tab), length(x))
})



