test_that("lc returns an object of class 'lc'", {
  expect_s3_class(lc(c(1,2,3,4,5)), "lc")
})

test_that("lc object has required components", {
  obj <- lc(c(1,2,3,4,5))
  expect_named(obj, c("p","L","L.general","Gini","x","n"))
})

test_that("lc p starts at 0 and ends at 1", {
  obj <- lc(runif(50))
  expect_equal(obj$p[1], 0)
  expect_equal(tail(obj$p, 1), 1)
})

test_that("lc L starts at 0 and ends at 1", {
  obj <- lc(runif(50))
  expect_equal(obj$L[1], 0)
  expect_equal(tail(obj$L, 1), 1)
})

test_that("lc L is monotonically non-decreasing", {
  obj <- lc(runif(100))
  expect_true(all(diff(obj$L) >= -1e-10))
})

test_that("lc Gini is in [0, 1]", {
  obj <- lc(runif(100))
  expect_gte(obj$Gini, 0); expect_lte(obj$Gini, 1)
})

test_that("lc Gini = 0 for a constant vector (perfect equality)", {
  obj <- lc(rep(5, 20))
  expect_equal(obj$Gini, 0, tolerance = 1e-10)
})

test_that("lc formula interface returns class lc or lclist", {
  set.seed(1)
  x <- rlnorm(60)
  g <- sample(c("a","b"), 60, replace=TRUE)
  df <- data.frame(x=x, g=g)
  res <- lc(x ~ g, data=df)
  expect_true(inherits(res, "lc") || inherits(res, "lclist"))
})

test_that("predict.lc returns a data frame with p and L columns", {
  obj <- lc(runif(50))
  pred <- predict(obj)
  expect_s3_class(pred, "data.frame")
  expect_named(pred, c("p","L"))
})

test_that("predict.lc with conf.level adds lci and uci columns", {
  set.seed(5)
  obj <- lc(rlnorm(50))
  pred <- predict(obj, conf.level=0.95, n=200)
  expect_named(pred, c("p","L","lci","uci"))
})

test_that("lc na.rm = TRUE removes NAs", {
  x <- c(1, 2, NA, 4, 5)
  expect_s3_class(lc(x, na.rm=TRUE), "lc")
})
