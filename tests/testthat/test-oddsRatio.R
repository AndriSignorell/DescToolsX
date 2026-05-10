

test_that("oddsRatio returns scalar estimate for table input", {
  
  x <- matrix(
    c(10, 20,
      5, 30),
    nrow = 2
  )
  
  res <- oddsRatio(x)
  
  expect_true(is.numeric(res))
  
  expect_equal(length(res), 1L)
  
})



test_that("oddsRatio returns named confidence interval vector", {
  
  x <- matrix(
    c(10, 20,
      5, 30),
    nrow = 2
  )
  
  res <- oddsRatio(
    x,
    conf.level = 0.95
  )
  
  expect_named(
    res,
    c("estimate", "lci", "uci")
  )
  
  expect_equal(length(res), 3L)
  
})



test_that("oddsRatio supports all table methods", {
  
  x <- matrix(
    c(10, 20,
      5, 30),
    nrow = 2
  )
  
  expect_no_error(
    oddsRatio(
      x,
      method = "wald",
      conf.level = 0.95
    )
  )
  
  expect_no_error(
    oddsRatio(
      x,
      method = "exact",
      conf.level = 0.95
    )
  )
  
  expect_no_error(
    oddsRatio(
      x,
      method = "midp",
      conf.level = 0.95
    )
  )
  
})



test_that("oddsRatio validates table input", {
  
  expect_error(
    oddsRatio("foo"),
    "Argument 'x' must be numeric."
  )
  
})



test_that("oddsRatio rejects missing values", {
  
  x <- matrix(
    c(1, 2,
      NA, 4),
    nrow = 2
  )
  
  expect_error(
    oddsRatio(x),
    "must not contain missing values"
  )
  
})



test_that("oddsRatio rejects non-2x2 matrices", {
  
  x <- matrix(1:9, nrow = 3)
  
  expect_error(
    oddsRatio(x),
    "must be a 2x2 matrix"
  )
  
})



test_that("oddsRatio rejects negative counts", {
  
  x <- matrix(
    c(1, -1,
      2, 3),
    nrow = 2
  )
  
  expect_error(
    oddsRatio(x),
    "must contain non-negative counts"
  )
  
})



test_that("oddsRatio rejects non-integer counts", {
  
  x <- matrix(
    c(1.5, 2,
      3, 4),
    nrow = 2
  )
  
  expect_error(
    oddsRatio(x),
    "must contain integer counts"
  )
  
})



test_that("oddsRatio rejects zero row totals", {
  
  x <- matrix(
    c(0, 0,
      1, 2),
    nrow = 2,
    byrow = TRUE
  )
  
  expect_error(
    oddsRatio(x),
    "must contain positive totals"
  )
  
})



test_that("oddsRatio accepts x and y input", {
  
  x <- c(1, 1, 0, 0, 1, 0)
  y <- c(1, 0, 1, 0, 1, 0)
  
  expect_no_error(
    oddsRatio(
      x,
      y,
      conf.level = 0.95
    )
  )
  
})



test_that("oddsRatio.glm returns OddsRatio object", {
  
  fit <- glm(
    vs ~ am,
    data = mtcars,
    family = binomial
  )
  
  res <- oddsRatio(fit)
  
  expect_s3_class(res, "OddsRatio")
  
})



test_that("oddsRatio.glm returns coefficient table", {
  
  fit <- glm(
    vs ~ am,
    data = mtcars,
    family = binomial
  )
  
  res <- oddsRatio(fit)
  
  expect_true(is.data.frame(res$coefficients))
  
  expect_named(
    res$coefficients,
    c(
      "term",
      "estimate",
      "logEstimate",
      "stdError",
      "pValue",
      "lci",
      "uci"
    )
  )
  
})



test_that("oddsRatio.glm supports both interval methods", {
  
  fit <- glm(
    vs ~ am,
    data = mtcars,
    family = binomial
  )
  
  expect_no_error(
    oddsRatio(
      fit,
      method = "wald"
    )
  )
  
  expect_no_error(
    oddsRatio(
      fit,
      method = "profile"
    )
  )
  
})



test_that("oddsRatio.glm rejects non-binomial models", {
  
  fit <- glm(
    mpg ~ wt,
    data = mtcars
  )
  
  expect_error(
    oddsRatio(fit),
    "Model must use binomial family."
  )
  
})



test_that("print.OddsRatio returns object invisibly", {
  
  fit <- glm(
    vs ~ am,
    data = mtcars,
    family = binomial
  )
  
  x <- oddsRatio(fit)
  
  expect_invisible(
    print(x)
  )
  
})

