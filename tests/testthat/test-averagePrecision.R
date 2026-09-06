test_that("averagePrecision computes known average precision", {
  resp <- c(0, 0, 1, 1)
  pred <- c(0.1, 0.4, 0.35, 0.8)

  expect_equal(averagePrecision(resp, pred), 5 / 6, tolerance = 1e-12)
})


test_that("averagePrecision handles tied scores as one threshold", {
  resp <- c(1, 0, 1, 0)
  pred <- c(0.8, 0.8, 0.4, 0.4)

  expect_equal(averagePrecision(resp, pred), 0.5, tolerance = 1e-12)
})


test_that("averagePrecision accepts arbitrary numeric scores", {
  resp <- c(0, 0, 1, 1)
  prob <- c(0.1, 0.4, 0.35, 0.8)
  score <- qlogis(prob)

  expect_equal(averagePrecision(resp, score),
               averagePrecision(resp, prob),
               tolerance = 1e-12)
})


test_that("averagePrecision works with glm objects", {
  dat <- data.frame(
    y = c(0, 0, 0, 1, 1, 1),
    x = c(-2, -1, 0, 0.5, 1, 2)
  )
  fit <- glm(y ~ x, data = dat, family = binomial)

  expect_equal(averagePrecision(fit),
               averagePrecision(fit$y, predict(fit, type = "response")),
               tolerance = 1e-12)
})


test_that("averagePrecision validates its inputs", {
  expect_error(averagePrecision(c(0, 1), c(0.2)), "same length", fixed = TRUE)
  expect_error(averagePrecision(c(0, NA, 1), c(0.1, 0.2, 0.9)),
               "must not contain missing values", fixed = TRUE)
  expect_error(averagePrecision(c(0, 2, 1), c(0.1, 0.2, 0.9)),
               "must be binary", fixed = TRUE)
  expect_error(averagePrecision(c(0, 1), c("low", "high")),
               "must be numeric", fixed = TRUE)
  expect_error(averagePrecision(numeric(), numeric()),
               "must not be empty", fixed = TRUE)
  expect_error(averagePrecision(c(0, 0), c(0.1, 0.2)),
               "at least one positive", fixed = TRUE)
})
