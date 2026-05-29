test_that("tschuprowT returns a single numeric", {
  tab <- matrix(c(10, 20, 30, 40), nrow=2)
  expect_length(tschuprowT(tab), 1)
  expect_true(is.numeric(tschuprowT(tab)))
})

test_that("tschuprowT is 0 for an independent table", {
  tab <- matrix(rep(25,4), nrow=2)
  expect_equal(tschuprowT(tab), 0, tolerance=1e-10)
})

test_that("tschuprowT is 1 for perfect association in a 2x2 table", {
  tab <- matrix(c(50,0,0,50), nrow=2)
  expect_equal(tschuprowT(tab), 1, tolerance=1e-10)
})

test_that("tschuprowT is non-negative", {
  tab <- matrix(c(10,20,30,40), nrow=2)
  expect_gte(tschuprowT(tab), 0)
})

test_that("tschuprowT equals cramerV for square tables", {
  # For square tables: sqrt((r-1)(c-1)) = r-1 = c-1 → T = V
  tab <- matrix(c(40,10,10,40), nrow=2)
  expect_equal(tschuprowT(tab), cramerV(as.table(tab)), tolerance=1e-8)
})

test_that("tschuprowT correct = TRUE returns value in [0, 1]", {
  tab <- matrix(c(10,5,5,30), nrow=2)
  res <- tschuprowT(tab, correct=TRUE)
  expect_gte(res, 0); expect_lte(res, 1)
})

test_that("tschuprowT accepts two vectors", {
  x <- c("A","A","B","B"); y <- c("yes","no","yes","no")
  expect_length(tschuprowT(x, y), 1)
})

test_that("tschuprowT manual formula matches function", {
  tab <- matrix(c(10,5,5,20), nrow=2)
  chi2 <- chisq.test(tab, correct=FALSE)$statistic
  df   <- prod(dim(tab)-1)
  expected <- sqrt(unname(chi2)/(sum(tab)*sqrt(df)))
  expect_equal(tschuprowT(tab), expected, tolerance=1e-10)
})
