
test_that("nmae returns 0 when predictions are perfect", {
  x     <- c(3, 5, 7)
  trainY <- c(3, 5, 7)
  expect_equal(nmae(x, ref = x, trainY = trainY), 0)
})

test_that("nmae is non-negative", {
  x      <- c(2.5, 3.0, 2.8)
  ref    <- c(3.0, 2.5, 3.0)
  trainY <- c(2, 3, 4, 3)
  expect_gte(nmae(x, ref, trainY), 0)
})

test_that("nmae returns NA when denominator is 0 (ref equals mean of trainY)", {
  # den = sum(abs(ref - mean(trainY))) = 0 only when ref[i] == mean(trainY) for all i
  x      <- c(1, 2, 3)
  ref    <- c(5, 5, 5)   # all ref equal mean(trainY) = 5
  trainY <- c(5, 5, 5)
  expect_true(is.na(nmae(x, ref, trainY)))
})

test_that("nmae manual calculation matches function", {
  x      <- c(2.5, 3.0, 2.8)
  ref    <- c(3.0, 2.5, 3.0)
  trainY <- c(2, 3, 4, 3)
  expected <- sum(abs(ref - x)) / sum(abs(ref - mean(trainY)))
  expect_equal(nmae(x, ref, trainY), expected, tolerance = 1e-10)
})

test_that("nmae stops when x and ref have different lengths", {
  expect_error(nmae(1:3, ref = 1:4, trainY = 1:3), "same length")
})

test_that("nmae returns 0 for perfect predictions when den != 0", {
  # sae = 0 (perfect predictions), den != 0 (ref != mean(trainY))
  x      <- c(2, 3, 4)
  ref    <- c(2, 3, 4)
  trainY <- c(1, 2, 3, 4, 5)   # mean = 3, ref != 3 for all elements
  expect_equal(nmae(x, ref, trainY), 0)
})


test_that("nmae and nmse honour na.rm like the rest of their family", {
  
  x   <- c(2.5, 3.0, NA, 2.8)
  ref <- c(3.0, 2.5, 3.0, 3.0)
  tr  <- c(2, 3, 4, 3)
  
  expect_true(is.na(nmae(x, ref, tr)))
  expect_true(is.na(nmse(x, ref, tr)))
  
  ok <- !is.na(x)
  expect_equal(nmae(x, ref, tr, na.rm = TRUE),
               sum(abs(ref[ok] - x[ok])) / sum(abs(ref[ok] - mean(tr))))
  expect_equal(nmse(x, ref, tr, na.rm = TRUE),
               sum((ref[ok] - x[ok])^2) / sum((ref[ok] - mean(tr))^2))
  
  # a degenerate baseline still gives NA, not a division by zero
  expect_true(is.na(nmse(c(1, 2), c(3, 3), c(3, 3))))
})

