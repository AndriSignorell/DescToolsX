
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




test_that("pearsonCor keeps its shape when the correlation is NA", {
  
  x <- c(1, NA, 3, 4, 5)
  y <- c(2, 3, 4, 5, 6)
  
  # .pearsonCI() returned NULL here, so the function handed back nothing
  # instead of a named triple - and .assocsTab() reads the result
  # positionally
  res <- pearsonCor(x, y, conf.level = 0.95)
  
  expect_named(res, c("est", "lci", "uci"))
  expect_length(res, 3L)
  expect_true(all(is.na(res)))
  
  # na.rm gives a real answer
  ok <- !is.na(x)
  expect_equal(unname(pearsonCor(x, y, na.rm = TRUE)), cor(x[ok], y[ok]))
})


test_that("pearsonCor's one-sided intervals close at the range boundary", {
  
  set.seed(2)
  x <- rnorm(40); y <- x + rnorm(40)
  
  left  <- pearsonCor(x, y, conf.level = 0.95, sides = "left")
  right <- pearsonCor(x, y, conf.level = 0.95, sides = "right")
  
  # tanh(Inf) is 1, so this already held - assert it so it keeps holding
  expect_equal(unname(left[["uci"]]), 1)
  expect_equal(unname(right[["lci"]]), -1)
  expect_true(is.finite(left[["lci"]]))
})

