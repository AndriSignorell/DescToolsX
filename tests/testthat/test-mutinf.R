

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

