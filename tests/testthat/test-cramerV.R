test_that("cramerV returns 0 for independent variables", {
  # Perfectly flat table → statistical independence
  tab <- matrix(c(25, 25, 25, 25), nrow = 2)
  expect_equal(cramerV(as.table(tab)), 0)
})

test_that("cramerV returns 1 for perfect association (2x2)", {
  tab <- matrix(c(50, 0, 0, 50), nrow = 2)
  expect_equal(cramerV(as.table(tab)), 1)
})

test_that("cramerV result is in [0, 1]", {
  tab <- as.table(matrix(c(10, 20, 30, 40), nrow = 2))
  v <- cramerV(tab)
  expect_gte(v, 0)
  expect_lte(v, 1)
})

test_that("cramerV returns a single numeric when conf.level = NA", {
  tab <- as.table(matrix(c(10, 20, 30, 40), nrow = 2))
  v <- cramerV(tab, conf.level = NA)
  expect_length(v, 1)
  expect_true(is.numeric(v))
})

test_that("cramerV returns a 3-element named vector with conf.level", {
  tab <- as.table(matrix(c(10, 20, 30, 40), nrow = 2))
  res <- cramerV(tab, conf.level = 0.95)
  expect_length(res, 3)
  expect_named(res, c("est", "lci", "uci"))
})

test_that("cramerV CI: lci <= est <= uci", {
  tab <- as.table(matrix(c(10, 5, 5, 30), nrow = 2))
  res <- cramerV(tab, conf.level = 0.95)
  expect_lte(res["lci"], res["est"])
  expect_gte(res["uci"], res["est"])
})

test_that("cramerV correct = TRUE applies Bergsma bias correction", {
  tab <- as.table(matrix(c(30, 10, 10, 50), nrow = 2))
  v_raw  <- cramerV(tab, correct = FALSE)
  v_corr <- cramerV(tab, correct = TRUE)
  # both should be in [0, 1] - they may differ
  expect_gte(v_raw,  0); expect_lte(v_raw,  1)
  expect_gte(v_corr, 0); expect_lte(v_corr, 1)
})

test_that("cramerV accepts two vectors as x and y", {
  x <- factor(c("A", "A", "B", "B"))
  y <- factor(c("X", "Y", "X", "Y"))
  expect_length(cramerV(x, y), 1)
})

test_that("cramerV works for non-square tables", {
  tab <- as.table(matrix(c(10, 5, 5, 10, 8, 12), nrow = 2))
  v <- cramerV(tab)
  expect_gte(v, 0)
  expect_lte(v, 1)
})



test_that("cramerV keeps estimate and interval on the same scale", {
  
  tab <- as.table(rbind(c(26, 26, 23, 18,  9),
                        c( 6,  7,  9, 14, 23)))
  
  plain <- cramerV(tab, conf.level = 0.95)
  corr  <- cramerV(tab, conf.level = 0.95, correct = TRUE)
  
  # the corrected estimate is smaller, and so must its bounds be
  expect_lt(unname(corr[["est"]]), unname(plain[["est"]]))
  expect_lt(unname(corr[["lci"]]), unname(plain[["lci"]]))
  expect_lt(unname(corr[["uci"]]), unname(plain[["uci"]]))
  
  # and the interval still brackets its own estimate
  expect_lte(unname(corr[["lci"]]), unname(corr[["est"]]))
  expect_gte(unname(corr[["uci"]]), unname(corr[["est"]]))
})


test_that("cramerV rejects a misspelled method even without a CI", {
  tab <- table(c("a", "a", "b", "b"), c("x", "y", "x", "y"))
  expect_error(cramerV(tab, method = "nochisq"), "arg")
})

