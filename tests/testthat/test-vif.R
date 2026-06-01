test_that("vif returns a named numeric vector for all-numeric predictors", {
  fit <- lm(mpg ~ wt + cyl, data = mtcars)
  res <- vif(fit)
  expect_type(res, "double")
  expect_named(res, c("wt","cyl"))
})

test_that("vif values are >= 1 for numeric predictors", {
  fit <- lm(mpg ~ wt + cyl + hp, data = mtcars)
  res <- vif(fit)
  expect_true(all(res >= 1))
})

test_that("vif returns a matrix when a factor term is present", {
  fit <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
  res <- vif(fit)
  expect_true(is.matrix(res))
  expect_equal(colnames(res), c("GVIF","Df","GVIF^(1/(2*Df))"))
})

test_that("vif Df column equals number of coefficients per term", {
  fit <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
  res <- vif(fit)
  expect_equal(res["Species","Df"], nlevels(iris$Species) - 1)
})

test_that("vif stops for fewer than 2 terms", {
  fit <- lm(mpg ~ wt, data = mtcars)
  expect_error(vif(fit), "fewer than 2")
})

test_that("vif stops for unsupported fitel type", {
  expect_error(vif(list(a=1)), "Unsupported")
})

test_that("vif warns when fitel has no intercept", {
  fit <- lm(mpg ~ 0 + wt + cyl, data = mtcars)
  expect_warning(vif(fit), "intercept")
})

test_that("vif values are close to car::vif for known fitel", {
  fit <- lm(mpg ~ wt + cyl + hp, data = mtcars)
  res <- vif(fit)
  # wt and cyl are fiterately correlated; all VIFs should be < 15
  expect_true(all(res < 15))
  expect_true(all(res > 1))
})
