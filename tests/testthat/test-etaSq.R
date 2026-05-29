# ---- shared setup ----
outcome    <- c(1.4, 2.1, 3.0, 2.1, 3.2, 4.7, 3.5, 4.5, 5.4)
treatment1 <- factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3))
anova1     <- aov(outcome ~ treatment1)

treatment2 <- factor(c(1, 2, 3, 1, 2, 3, 1, 2, 3))
anova2     <- aov(outcome ~ treatment1 + treatment2)

test_that("etaSq.lm returns a matrix with 2 columns for one-way ANOVA", {
  E <- etaSq(anova1)
  expect_true(is.matrix(E))
  expect_equal(ncol(E), 2L)
})

test_that("etaSq.lm column names are eta.sq and eta.sq.part", {
  E <- etaSq(anova1)
  expect_equal(colnames(E), c("eta.sq", "eta.sq.part"))
})

test_that("etaSq eta.sq values are in [0, 1]", {
  E <- etaSq(anova1)
  expect_true(all(E[, "eta.sq"] >= 0 & E[, "eta.sq"] <= 1))
})

test_that("etaSq partial eta.sq values are in [0, 1]", {
  E <- etaSq(anova1)
  expect_true(all(E[, "eta.sq.part"] >= 0 & E[, "eta.sq.part"] <= 1))
})

test_that("etaSq eta.sq sums to <= 1 across all terms (one-way)", {
  E <- etaSq(anova1)
  expect_lte(sum(E[, "eta.sq"]), 1 + 1e-9)
})

test_that("etaSq type = 1 returns the same structure as type = 2", {
  E1 <- etaSq(anova1, type = 1)
  E2 <- etaSq(anova1, type = 2)
  expect_equal(dim(E1), dim(E2))
})

test_that("etaSq type = 3 works for one-way ANOVA", {
  E3 <- etaSq(anova1, type = 3)
  expect_true(is.matrix(E3))
})

test_that("etaSq anova = TRUE adds SS, df, MS, F, p columns", {
  E <- etaSq(anova1, anova = TRUE)
  expect_true(all(c("SS","df","MS","F","p") %in% colnames(E)))
})

test_that("etaSq works for two-way ANOVA", {
  E <- etaSq(anova2)
  expect_equal(nrow(E), 2L)   # two terms: treatment1 and treatment2
})

test_that("etaSq row names match the ANOVA term labels", {
  E <- etaSq(anova2)
  expect_true(all(rownames(E) %in% c("treatment1","treatment2")))
})

test_that("etaSq stops with invalid type", {
  expect_error(etaSq(anova1, type = 99))
})

test_that("etaSq stops with non-logical anova argument", {
  expect_error(etaSq(anova1, anova = "yes"))
})
