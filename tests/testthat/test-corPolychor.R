test_that("corPolychor returns a numeric value in [-1, 1]", {
  set.seed(1)
  x <- factor(sample(1:4, 100, replace = TRUE), ordered = TRUE)
  y <- factor(sample(1:4, 100, replace = TRUE), ordered = TRUE)
  rho <- corPolychor(x, y)
  expect_length(rho, 1)
  expect_true(is.numeric(rho))
  expect_gte(rho, -1)
  expect_lte(rho,  1)
})

test_that("corPolychor is near 1 for perfectly correlated ordinal data", {
  x <- factor(1:5, ordered = TRUE)
  tab <- diag(5) * 20
  rho <- corPolychor(as.table(tab))
  expect_gt(rho, 0.95)
})

test_that("corPolychor is near -1 for perfectly anti-correlated ordinal data", {
  tab <- as.table(matrix(c(
    0, 0, 20,
    0, 20, 0,
    20, 0, 0), nrow = 3, byrow = TRUE))
  rho <- corPolychor(tab)
  expect_lt(rho, -0.95)
})

test_that("corPolychor is near 0 for independent ordinal data", {
  set.seed(99)
  x <- factor(sample(1:3, 300, replace = TRUE), ordered = TRUE)
  y <- factor(sample(1:3, 300, replace = TRUE), ordered = TRUE)
  rho <- corPolychor(x, y)
  expect_lt(abs(rho), 0.2)
})

test_that("corPolychor method = 'ml' returns a numeric in [-1, 1]", {
  set.seed(2)
  x <- factor(sample(1:3, 80, replace = TRUE), ordered = TRUE)
  y <- factor(sample(1:3, 80, replace = TRUE), ordered = TRUE)
  rho <- corPolychor(x, y, method = "ml")
  expect_gte(rho, -1)
  expect_lte(rho,  1)
})

test_that("corPolychor se = TRUE returns a list with expected components", {
  set.seed(3)
  x <- factor(sample(1:3, 80, replace = TRUE), ordered = TRUE)
  y <- factor(sample(1:3, 80, replace = TRUE), ordered = TRUE)
  res <- corPolychor(x, y, method = "ml", se = TRUE)
  expect_type(res, "list")
  expect_true(all(c("rho", "rowCuts", "colCuts", "var", "n") %in% names(res)))
})

test_that("corPolychor accepts a contingency table directly", {
  tab <- as.table(matrix(c(10, 5, 2, 5, 15, 5, 2, 5, 20), nrow = 3))
  rho <- corPolychor(tab)
  expect_gte(rho, -1)
  expect_lte(rho,  1)
})

test_that("corPolychor warns and removes empty rows/columns", {
  tab <- as.table(matrix(c(10, 0, 0, 0, 0, 0, 0, 10), nrow = 2,
                          dimnames = list(c("A","B"), c("X","Y","Z","W"))))
  # Expect warning about empty columns
  expect_warning(corPolychor(tab), "empty column")
})


test_that("corPolychor is not truncated at tanh(2)", {
  
  # two nearly identical ordinal items: the latent correlation is close
  # to 1 and used to saturate at 0.964, the boundary of the old c(-2, 2)
  # search interval
  set.seed(4)
  z <- rnorm(400)
  a <- cut(z, breaks = c(-Inf, -0.5, 0.5, Inf))
  b <- cut(z + rnorm(400, sd = 0.05), breaks = c(-Inf, -0.5, 0.5, Inf))
  
  rho <- corPolychor(a, b)
  
  expect_gt(rho, 0.97)
  expect_lt(rho, 1)
})


test_that("standard errors require ml", {
  set.seed(5)
  a <- factor(sample(1:3, 100, replace = TRUE), ordered = TRUE)
  b <- factor(sample(1:3, 100, replace = TRUE), ordered = TRUE)
  
  expect_error(corPolychor(a, b, method = "two-step", se = TRUE), "ml")
  
  res <- corPolychor(a, b, method = "ml", se = TRUE)
  expect_s3_class(res, "Polychor")
  expect_identical(res$method, "ml")
})


