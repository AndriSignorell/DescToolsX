
test_that("pearsonCor returns a numeric value in [-1, 1]", {
  res <- with(swiss, pearsonCor(Fertility, Agriculture))
  expect_length(res, 1)
  expect_gte(res, -1); expect_lte(res, 1)
})

test_that("pearsonCor matches cor() for two vectors", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(2, 4, 5, 4, 5)
  expect_equal(pearsonCor(x, y), cor(x, y), tolerance = 1e-10)
})

test_that("pearsonCor is 1 for perfectly correlated data", {
  x <- 1:10
  expect_equal(pearsonCor(x, x), 1, tolerance = 1e-10)
})

test_that("pearsonCor is -1 for perfectly anti-correlated data", {
  x <- 1:10
  expect_equal(pearsonCor(x, -x), -1, tolerance = 1e-10)
})

test_that("pearsonCor with conf.level returns named vector est/lci/uci", {
  res <- with(swiss, pearsonCor(Fertility, Agriculture, conf.level = 0.95))
  expect_length(res, 3)
  expect_named(res, c("est","lci","uci"))
})

test_that("pearsonCor CI: lci < est < uci", {
  res <- with(swiss, pearsonCor(Fertility, Agriculture, conf.level = 0.95))
  expect_lt(res["lci"], res["est"])
  expect_gt(res["uci"], res["est"])
})

test_that("pearsonCor returns NA when NA present and na.rm = FALSE", {
  x <- c(1, 2, NA, 4)
  y <- c(1, 2, 3,  4)
  expect_true(is.na(pearsonCor(x, y)))
})

test_that("pearsonCor na.rm = TRUE strips NA pairs", {
  x <- c(1, 2, NA, 4)
  y <- c(1, 2,  3, 4)
  res_narm  <- pearsonCor(x, y, na.rm = TRUE)
  res_clean <- pearsonCor(c(1,2,4), c(1,2,4))
  expect_equal(res_narm, res_clean, tolerance = 1e-10)
})

test_that("pearsonCor sides = 'left' gives one-sided CI (uci = 1 after tanh)", {
  # Fisher Z CI uses tanh() → tanh(Inf) = 1, not Inf
  res <- with(swiss, pearsonCor(Fertility, Agriculture,
                                conf.level=0.95, sides="left"))
  expect_equal(unname(res["uci"]), 1, tolerance=1e-10)
})

