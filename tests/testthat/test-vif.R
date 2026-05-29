test_that("vif returns a named numeric vector for all-numeric predictors", {
  mod <- lm(mpg ~ wt + cyl, data = mtcars)
  res <- vif(mod)
  expect_type(res, "double")
  expect_named(res, c("wt","cyl"))
})

test_that("vif values are >= 1 for numeric predictors", {
  mod <- lm(mpg ~ wt + cyl + hp, data = mtcars)
  res <- vif(mod)
  expect_true(all(res >= 1))
})

test_that("vif returns a matrix when a factor term is present", {
  mod <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
  res <- vif(mod)
  expect_true(is.matrix(res))
  expect_equal(colnames(res), c("GVIF","Df","GVIF^(1/(2*Df))"))
})

test_that("vif Df column equals number of coefficients per term", {
  mod <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
  res <- vif(mod)
  expect_equal(res["Species","Df"], nlevels(iris$Species) - 1)
})

test_that("vif stops for fewer than 2 terms", {
  mod <- lm(mpg ~ wt, data = mtcars)
  expect_error(vif(mod), "fewer than 2")
})

test_that("vif stops for unsupported model type", {
  expect_error(vif(list(a=1)), "Unsupported")
})

test_that("vif warns when model has no intercept", {
  mod <- lm(mpg ~ 0 + wt + cyl, data = mtcars)
  expect_warning(vif(mod), "intercept")
})

test_that("vif values are close to car::vif for known model", {
  mod <- lm(mpg ~ wt + cyl + hp, data = mtcars)
  res <- vif(mod)
  # wt and cyl are moderately correlated; all VIFs should be < 15
  expect_true(all(res < 15))
  expect_true(all(res > 1))
})
