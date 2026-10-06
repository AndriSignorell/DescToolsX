test_that("averagePrecision computes known average precision", {
  resp <- c(0, 0, 1, 1)
  pred <- c(0.1, 0.4, 0.35, 0.8)

  expect_equal(averagePrecision(pred, resp), 5 / 6, tolerance = 1e-12)
})


test_that("averagePrecision handles tied scores as one threshold", {
  resp <- c(1, 0, 1, 0)
  pred <- c(0.8, 0.8, 0.4, 0.4)

  expect_equal(averagePrecision(pred, resp), 0.5, tolerance = 1e-12)
})


test_that("averagePrecision accepts arbitrary numeric scores", {
  resp <- c(0, 0, 1, 1)
  prob <- c(0.1, 0.4, 0.35, 0.8)
  score <- qlogis(prob)

  expect_equal(averagePrecision(score, resp),
               averagePrecision(prob, resp),
               tolerance = 1e-12)
})


test_that("averagePrecision works with glm objects", {
  dat <- data.frame(
    # overlapping classes: with complete separation glm() warns that the
    # fitted probabilities are numerically 0 or 1
    y = c(0, 0, 1, 0, 1, 1),
    x = c(-2, -1, 0, 0.5, 1, 2)
  )
  fit <- glm(y ~ x, data = dat, family = binomial)

  expect_equal(averagePrecision(fit),
               averagePrecision(predict(fit, type = "response"), fit$y),
               tolerance = 1e-12)
})


test_that("averagePrecision validates its inputs", {
  expect_error(averagePrecision(c(0.2), c(0, 1)), "same length", fixed = TRUE)
  expect_error(averagePrecision(c(0.1, 0.2, 0.9), c(0, NA, 1)),
               "must not contain missing values", fixed = TRUE)
  expect_error(averagePrecision(c(0.1, 0.2, 0.9), c(0, 2, 1)),
               "must be binary", fixed = TRUE)
  expect_error(averagePrecision(c("low", "high"), c(0, 1)),
               "must be numeric", fixed = TRUE)
  expect_error(averagePrecision(numeric(), numeric()),
               "must not be empty", fixed = TRUE)
  expect_error(averagePrecision(c(0.1, 0.2), c(0, 0)),
               "at least one positive", fixed = TRUE)
})
