# Shannon entropy: explicit expectations replace exploratory top-level calls.

test_that("entropy matches known distributions and logarithm bases", {
  expect_equal(entropy(c(1, 1)), 1)
  expect_equal(entropy(c(10, 0, 0, 0)), 0)
  expect_equal(entropy(rep(1, 4)), 2)
  expect_equal(entropy(rep(1, 4), base = exp(1)), log(4))
  expect_equal(entropy(rep(1, 4), base = 10), log10(4))
  expect_equal(entropy(rep(1, 4), normalize = TRUE), 1)
  expect_equal(entropy(c(10, 0, 0, 0), normalize = TRUE), 0)
  # Empty levels are not counted in the normalization denominator.
  expect_equal(entropy(c(2, 2, 0, 0), normalize = TRUE), 1)
  expected <- -(0.75 * log2(0.75) + 0.25 * log2(0.25))
  expect_equal(entropy(c(3, 1)), expected)
  expect_equal(entropy(c(30, 10)), expected)
})

test_that("entropy accepts counts, tables, matrices and arrays", {
  counts <- c(10, 20, 30, 40)
  p <- counts / sum(counts)
  expected <- -sum(p * log2(p))
  for (x in list(counts, as.table(counts), matrix(counts, 2),
                 array(counts, dim = c(2, 1, 2)))) {
    ans <- entropy(x)
    expect_type(ans, "double")
    expect_length(ans, 1L)
    expect_equal(ans, expected)
  }
})

test_that("entropy tabulates categorical vectors and joint observations", {
  x <- c("A", "A", "B", "B", "C")
  expected <- -sum(c(0.4, 0.4, 0.2) * log2(c(0.4, 0.4, 0.2)))
  expect_equal(entropy(x), expected)
  expect_equal(entropy(factor(x, levels = c("A", "B", "C", "unused"))), expected)
  expect_equal(entropy(c(TRUE, FALSE)), 1)
  y <- c("X", "X", "X", "Y", "Y")
  # Joint occupied counts: 2, 1, 1, 1.
  expect_equal(entropy(x, y), -sum(c(0.4, 0.2, 0.2, 0.2) *
                                  log2(c(0.4, 0.2, 0.2, 0.2))))
  expect_equal(entropy(x, y), entropy(table(x, y)))
  expect_equal(entropy(c("A", NA), useNA = "ifany"), 1)
  expect_equal(entropy(c("A", NA)), 0)
})

test_that("entropy handles missing, empty and zero-total counts", {
  expect_identical(entropy(c(1, NA_real_, 1)), NA_real_)
  expect_equal(entropy(c(1, NA_real_, 1), na.rm = TRUE), 1)
  expect_equal(entropy(c(1, NaN, 1), na.rm = TRUE), 1)
  expect_identical(entropy(c(NA_real_, NaN), na.rm = TRUE), NA_real_)
  expect_identical(entropy(numeric()), NA_real_)
  expect_identical(entropy(c(0, 0)), NA_real_)
  expect_identical(entropy(c(0, 0), normalize = TRUE), NA_real_)
  expect_error(entropy(c(1, -1)), "non-negative counts")
})

test_that("entropy rejects invalid logarithm bases", {
  for (bad in list("2", numeric(), c(2, 10), NA_real_, NaN,
                  Inf, -Inf, 0, -2, 1)) {
    expect_error(entropy(c(1, 1), base = bad), "single positive number other than 1")
  }
})

# Preserve the mutual-information checks from the supplied file,
# replacing bare examples and stopifnot with testthat assertions.
test_that("mutInf handles independence, perfect association and symmetry", {
  independent <- matrix(25, 2, 2)
  perfect <- diag(c(50, 50))
  tab <- matrix(c(10, 20, 30, 40), 2)
  expect_equal(mutInf(independent), 0, tolerance = 1e-12)
  expect_equal(mutInf(perfect), 1)
  expect_equal(mutInf(perfect, normalize = TRUE), 1)
  expect_equal(mutInf(independent, normalize = TRUE), 0, tolerance = 1e-12)
  expect_equal(mutInf(tab), mutInf(t(tab)))
  x <- c("A", "A", "A", "B", "B", "B")
  y <- c("X", "X", "Y", "Y", "Y", "X")
  expect_equal(mutInf(x, y), mutInf(table(x, y)))
})

test_that("mutInf and entropy satisfy their information bounds", {
  set.seed(1)
  tab <- matrix(sample(1:100, 25, TRUE), nrow = 5)
  mi <- mutInf(tab)
  expect_type(mi, "double")
  expect_length(mi, 1L)
  expect_gte(mi, -1e-12)
  expect_lte(mi, min(entropy(rowSums(tab)), entropy(colSums(tab))) + 1e-12)
  normalized <- mutInf(tab, normalize = TRUE)
  expect_gte(normalized, -1e-12)
  expect_lte(normalized, 1 + 1e-12)
  counts <- c(2, 5, 8, 1, 4)
  expect_gte(entropy(counts), 0)
  expect_lte(entropy(counts), log2(length(counts)))
})
