test_that("contCoef returns a numeric scalar", {
  tab <- as.table(matrix(c(10, 20, 30, 40), nrow = 2))
  cc <- contCoef(tab)
  expect_length(cc, 1)
  expect_true(is.numeric(cc))
})

test_that("contCoef result is in [0, 1]", {
  tab <- as.table(matrix(c(10, 20, 30, 40), nrow = 2))
  cc <- contCoef(tab)
  expect_gte(cc, 0)
  expect_lte(cc, 1)
})

test_that("contCoef is near 0 for an independent table", {
  tab <- as.table(matrix(rep(25, 4), nrow = 2))
  cc <- contCoef(tab)
  expect_lt(cc, 0.05)
})

test_that("contCoef is larger for a more associated table", {
  tab_weak   <- as.table(matrix(c(25, 25, 25, 25), nrow = 2))
  tab_strong <- as.table(matrix(c(48,  2,  2, 48), nrow = 2))
  expect_lt(contCoef(tab_weak), contCoef(tab_strong))
})

test_that("contCoef correct = TRUE returns value in [0, 1]", {
  tab <- as.table(matrix(c(10, 5, 5, 30), nrow = 2))
  cc_corr <- contCoef(tab, correct = TRUE)
  expect_gte(cc_corr, 0)
  expect_lte(cc_corr, 1)
})

test_that("contCoef accepts x and y vectors", {
  x <- factor(c("A", "A", "B", "B"))
  y <- factor(c("X", "Y", "X", "Y"))
  cc <- contCoef(x, y)
  expect_length(cc, 1)
  expect_gte(cc, 0)
})

test_that("contCoef with conf.level returns named vector est/lci/uci", {
  tab <- as.table(matrix(c(10, 5, 5, 30), nrow = 2))
  res <- contCoef(tab, conf.level = 0.95, R = 200)
  expect_length(res, 3)
  expect_named(res, c("est", "lci", "uci"))
})

test_that("contCoef CI: lci <= est <= uci", {
  tab <- as.table(matrix(c(10, 5, 5, 30), nrow = 2))
  res <- contCoef(tab, conf.level = 0.95, R = 200)
  expect_lte(res["lci"], res["est"])
  expect_gte(res["uci"], res["est"])
})
