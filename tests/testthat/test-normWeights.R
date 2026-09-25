# .normWeights() has been covered only through its callers (meanX, madX,
# meanAD, ...); these tests pin its own contract.

test_that("it returns x, the weights as doubles and their sum", {
  z <- .normWeights(c(3, 1, 2), c(1L, 2L, 3L))
  expect_named(z, c("x", "weights", "wsum"))
  expect_identical(z$x, c(3, 1, 2))
  expect_identical(z$weights, c(1, 2, 3))
  expect_equal(z$wsum, 6)
})

test_that("zero weights are dropped, the sum is unchanged", {
  z <- .normWeights(1:4, c(1, 0, 2, 0))
  expect_identical(z$x, c(1L, 3L))
  expect_identical(z$weights, c(1, 2))
  expect_equal(z$wsum, 3)
})

test_that("normwt rescales to the number of observations kept", {
  z <- .normWeights(1:3, c(2, 2, 4), normwt = TRUE)
  expect_equal(z$weights, c(2, 2, 4) * 3 / 8)
  expect_equal(sum(z$weights), 3)
  expect_equal(z$wsum, 8)
  # without normwt the weights stay as given
  expect_identical(.normWeights(1:3, c(2, 2, 4))$weights, c(2, 2, 4))
})

test_that("missing values: NA shape without na.rm, pairwise removal with it", {
  z <- .normWeights(c(1, NA, 3), c(1, 1, 1))
  expect_named(z, c("x", "weights", "wsum"))
  expect_true(all(is.na(unlist(z))))
  expect_true(is.na(.normWeights(c(1, 2, 3), c(1, NA, 1))$wsum))

  z <- .normWeights(c(1, NA, 3, 4), c(2, 1, NA, 1), na.rm = TRUE)
  expect_identical(z$x, c(1, 4))
  expect_identical(z$weights, c(2, 1))
  expect_equal(z$wsum, 3)
})

test_that("nothing left after na.rm gives an empty result, not an error", {
  z <- .normWeights(c(NA_real_, NA_real_), c(1, 1), na.rm = TRUE)
  expect_length(z$x, 0L)
  expect_length(z$weights, 0L)
  expect_true(is.nan(z$wsum))
})

test_that("invalid weights are refused", {
  expect_error(.normWeights(1:3, NULL), "must not be NULL")
  expect_error(.normWeights(1:3, 1:2), "length")
  expect_error(.normWeights(1:3, c("1", "2", "3")), "numeric")
  expect_error(.normWeights(1:3, c(1, -1, 1)), "non-negative")
  expect_error(.normWeights(1:3, c(1, Inf, 1)), "finite")
  expect_error(.normWeights(1:3, c(0, 0, 0)), "all zero")
})
