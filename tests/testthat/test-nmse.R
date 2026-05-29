
test_that("nmse returns 0 for perfect predictions", {
  x <- c(1, 2, 3)
  expect_equal(nmse(x, ref = x, trainY = x), 0)
})

test_that("nmse is non-negative", {
  x      <- c(2.5, 3.0, 2.8)
  ref    <- c(3.0, 2.5, 3.0)
  trainY <- c(2, 3, 4, 3)
  expect_gte(nmse(x, ref, trainY), 0)
})

test_that("nmse returns NA when denominator is 0 (ref equals mean of trainY)", {
  # den = sum((ref - mean(trainY))^2) = 0 only when ref[i] == mean(trainY) for all i
  x      <- c(1, 2, 3)
  ref    <- c(5, 5, 5)   # all ref equal mean(trainY) = 5
  trainY <- c(5, 5, 5)
  expect_true(is.na(nmse(x, ref, trainY)))
})

test_that("nmse manual calculation matches function", {
  x      <- c(2.5, 3.0, 2.8)
  ref    <- c(3.0, 2.5, 3.0)
  trainY <- c(2, 3, 4, 3)
  expected <- sum((ref-x)^2) / sum((ref - mean(trainY))^2)
  expect_equal(nmse(x, ref, trainY), expected, tolerance = 1e-10)
})

test_that("nmse stops when x and ref have different lengths", {
  expect_error(nmse(1:3, ref = 1:4, trainY = 1:3), "same length")
})

test_that("nmse > 0 for predictions with large errors when den != 0", {
  # large single error → SSE big, den != 0 (ref[i] != mean(trainY))
  x      <- c(1, 10)
  ref    <- c(2, 2)    # mean(trainY) = 2 → ref[2]=2 contributes 0; ref[1]=2 contributes 0
  trainY <- c(1, 2, 3) # mean = 2; ref = c(2,2) → den = 0 → pick ref != mean
  # Use ref where not all equal mean(trainY)
  x2      <- c(1, 10)
  ref2    <- c(3, 4)
  trainY2 <- c(1, 2, 3)  # mean = 2; ref2 != 2 → den > 0
  expect_gt(nmse(x2, ref2, trainY2), 0)
})

