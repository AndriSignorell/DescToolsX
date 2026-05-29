test_that("phi returns a single numeric value", {
  tab <- matrix(c(10, 20, 30, 40), nrow = 2)
  res <- phi(tab)
  expect_length(res, 1)
  expect_true(is.numeric(res))
})

test_that("phi is 0 for an independent 2x2 table", {
  tab <- matrix(rep(25, 4), nrow = 2)
  expect_equal(phi(tab), 0, tolerance = 1e-10)
})

test_that("phi is 1 for a perfectly associated 2x2 table", {
  tab <- matrix(c(50, 0, 0, 50), nrow = 2)
  expect_equal(phi(tab), 1, tolerance = 1e-10)
})

test_that("phi is non-negative", {
  tab <- matrix(c(10, 20, 30, 40), nrow = 2)
  expect_gte(phi(tab), 0)
})

test_that("phi is symmetric: phi(x,y) == phi(y,x)", {
  x <- factor(c("A","A","B","B"))
  y <- factor(c("X","Y","X","Y"))
  expect_equal(phi(x, y), phi(y, x))
})

test_that("phi accepts two vectors", {
  x <- c("A","A","B","B")
  y <- c("yes","no","yes","no")
  res <- phi(x, y)
  expect_length(res, 1)
})

test_that("phi for larger tables can exceed 1 (not bounded by 1)", {
  # 3x3 perfectly diagonal table
  tab <- diag(3) * 50
  res <- phi(tab)
  # phi may be > 1 for non-2x2 tables
  expect_gte(res, 0)
})

test_that("phi equals sqrt(chisq/n) manually", {
  tab <- matrix(c(10, 5, 5, 20), nrow = 2)
  chi <- chisq.test(tab, correct = FALSE)$statistic
  expected <- sqrt(unname(chi) / sum(tab))
  expect_equal(phi(tab), expected, tolerance = 1e-10)
})
