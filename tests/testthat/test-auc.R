
library(testthat)

test_that("auc trapezoid works for simple case", {
  x <- c(1, 2, 3)
  y <- c(0, 1, 0)
  
  res <- auc(x, y)
  expect_equal(res, 1)
})

test_that("auc step method works", {
  x <- c(1, 2, 3)
  y <- c(0, 1, 0)
  
  res <- auc(x, y, method = "step")
  expect_equal(res, 1)
})

test_that("auc spline runs without error", {
  x <- c(1, 2, 3, 4)
  y <- c(0, 1, 0, 1)
  
  res <- auc(x, y, method = "spline")
  expect_true(is.numeric(res))
})

test_that("auc handles unsorted x", {
  x <- c(3, 1, 2)
  y <- c(0, 0, 1)
  
  res <- auc(x, y)
  expect_true(is.numeric(res))
})

test_that("auc absoluteArea works", {
  x <- c(1, 2, 3)
  y <- c(-1, 1, -1)
  
  res <- auc(x, y, absoluteArea = TRUE)
  expect_true(res > 0)
})

test_that("auc respects from/to limits", {
  x <- 1:5
  y <- c(0, 1, 1, 1, 0)
  
  full <- auc(x, y)
  partial <- auc(x, y, from = 2, to = 4)
  
  expect_true(partial < full)
})

test_that("auc removes NA when na.rm = TRUE", {
  x <- c(1, 2, 3)
  y <- c(0, NA, 1)
  
  res <- auc(x, y, na.rm = TRUE)
  expect_true(is.numeric(res))
})

test_that("auc returns NA for too short input", {
  expect_true(is.na(auc(1, 1)))
})

test_that("auc errors on unequal length", {
  expect_error(auc(1:3, 1:2))
})


# Additional branch coverage and reference checks
test_that("auc validates numeric inputs, flags and spline subdivisions", {
  expect_error(auc(letters[1:3], 1:3), "numeric vectors")
  expect_error(auc(1:3, letters[1:3]), "numeric vectors")
  for (bad in list(1, NA, logical(), c(TRUE, FALSE))) {
    expect_error(auc(1:3, 1:3, absoluteArea = bad), "absoluteArea")
    expect_error(auc(1:3, 1:3, na.rm = bad), "na.rm")
  }
  for (bad in list("2", numeric(), c(1, 2), NA_real_, Inf, 0, 1.5))
    expect_error(auc(1:3, 1:3, method = "spline", subdivisions = bad), "subdivisions")
  expect_error(auc(c(1, Inf), 1:2), "finite values")
  expect_error(auc(1:2, c(1, Inf)), "finite values")
  expect_error(auc(c(1, 1, 2), 1:3), "unique")
})

test_that("auc validates integration limits and handles missing data", {
  for (bad in list("1", numeric(), c(1, 2), NA_real_, Inf)) {
    expect_error(auc(0:2, 0:2, from = bad), "'from'")
    expect_error(auc(0:2, 0:2, to = bad), "'to'")
  }
  expect_error(auc(0:2, 0:2, from = 2, to = 1), "greater")
  expect_error(auc(0:2, 0:2, from = -1), "within the range")
  expect_error(auc(0:2, 0:2, to = 3), "within the range")
  expect_equal(auc(0:2, 0:2, from = 1, to = 1), 0)
  expect_identical(auc(c(0, NA, 2), 0:2), NA_real_)
  expect_identical(auc(0:2, c(0, NA, 2)), NA_real_)
  expect_identical(auc(c(NA_real_, 1), c(0, NA_real_), na.rm = TRUE), NA_real_)
  expect_equal(auc(c(NA, 0, 2), c(99, 0, 2), na.rm = TRUE), 2)
})

test_that("auc absolute areas split crossings and integrate clipped intervals", {
  expect_equal(auc(c(0, 3), c(-1, 2)), 1.5)
  expect_equal(auc(c(0, 3), c(-1, 2), absoluteArea = TRUE), 2.5)
  expect_equal(auc(c(0, 3), c(1, -2), absoluteArea = TRUE), 2.5)
  expect_equal(auc(0:2, c(-2, 0, -2), absoluteArea = TRUE), 2)
  expect_equal(auc(0:2, c(-2, 3, 8), method = "step", absoluteArea = TRUE), 5)
  expect_equal(auc(0:2, c(-2, 3, 8), method = "step", from = 0.5, to = 1.5), 0.5)
  expect_equal(auc(c(0, 2), c(0, 2), from = 0.5, to = 1.5), 1)
  expect_equal(auc(c(0, 2), c(-1, 1), method = "spline", absoluteArea = TRUE), 1, tolerance = 1e-7)
})
