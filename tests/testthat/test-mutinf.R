

test_that("mutInf is non-negative and matches its definition", {
  
  tab <- matrix(c(10, 20, 30, 40), nrow = 2)
  
  hx <- entropy(rowSums(tab))
  hy <- entropy(colSums(tab))
  expect_equal(mutInf(tab), max(hx + hy - entropy(tab), 0))
  
  # independence gives zero, not a small negative number
  indep <- outer(c(30, 70), c(40, 60)) / 100
  expect_equal(mutInf(indep), 0)
  expect_gte(mutInf(indep), 0)
  
  # perfect dependence: MI equals the common entropy
  diagTab <- diag(c(25, 25, 25, 25))
  expect_equal(mutInf(diagTab), entropy(rep(25, 4)))
  expect_equal(mutInf(diagTab, normalize = TRUE), 1)
  
  # a degenerate margin carries no information
  expect_equal(mutInf(matrix(c(50, 50, 0, 0), nrow = 2), normalize = TRUE), 0)
  
  expect_error(mutInf(matrix(c(1, -1, 2, 3), nrow = 2)), "non-negative")
})


test_that("mutInf validates counts and returns NA for missing or empty totals", {
  expect_error(mutInf(matrix(letters[1:4], 2)), "numeric table")
  expect_error(mutInf(matrix(c(1, -1, 2, 3), 2)), "non-negative")
  expect_identical(mutInf(matrix(c(1, NA, 2, 3), 2)), NA_real_)
  expect_identical(mutInf(matrix(0, 2, 3)), NA_real_)
  expect_identical(mutInf(matrix(numeric(), 0, 2)), NA_real_)
})

test_that("mutInf matches the direct probability-ratio formula", {
  tab <- matrix(c(12, 3, 5, 20, 0, 10), nrow = 2)
  p <- tab / sum(tab)
  independent <- outer(rowSums(p), colSums(p))
  keep <- p > 0
  expected <- sum(p[keep] * log2(p[keep] / independent[keep]))
  expect_equal(mutInf(tab), expected)
  expect_equal(mutInf(tab, base = exp(1)), expected * log(2))
  expect_equal(mutInf(t(tab)), expected)
  expect_equal(mutInf(10 * tab), expected)
  hx <- -sum(rowSums(p) * log2(rowSums(p)))
  hy <- -sum(colSums(p) * log2(colSums(p)))
  expect_equal(mutInf(tab, normalize = TRUE), expected / sqrt(hx * hy))
})

test_that("mutInf tabulates paired vectors and forwards table arguments", {
  x <- c("a", "a", "b", "b")
  expect_equal(mutInf(x, x), 1)
  expect_equal(mutInf(c("a", NA), c("a", NA), useNA = "ifany"), 1)
  expect_equal(mutInf(matrix(c(2, 3, 5), nrow = 1), normalize = TRUE), 0)
  expect_equal(mutInf(matrix(c(2, 3, 5), ncol = 1), normalize = TRUE), 0)
})
