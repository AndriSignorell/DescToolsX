

# perfect agreement
.perfect <- matrix(c(1,1,1, 2,2,2, 3,3,3), ncol=3, byrow=TRUE)
# partial agreement
.partial <- matrix(c(1,1,1, 2,2,2, 1,2,1, 3,3,3, 2,2,1), ncol=3, byrow=TRUE)

test_that("randolphKappa returns a single numeric", {
  res <- randolphKappa(.partial)
  expect_length(res, 1); expect_true(is.numeric(res))
})

test_that("randolphKappa = 1 for perfect agreement", {
  expect_equal(randolphKappa(.perfect), 1, tolerance = 1e-10)
})

test_that("randolphKappa result is in [-1, 1]", {
  res <- randolphKappa(.partial)
  expect_gte(res, -1); expect_lte(res, 1)
})

test_that("randolphKappa perfect > partial agreement", {
  expect_gt(randolphKappa(.perfect), randolphKappa(.partial))
})

test_that("randolphKappa = 0 when every subject has one vote per category", {
  # When each row is a permutation of all k categories, max(tab) = 1 for every
  # subject, so Po = 1/k exactly and kappa = (Po - 1/k) / (1 - 1/k) = 0.
  # This tests the zero-agreement boundary without relying on large-sample
  # convergence of random sampling (which is very slow for small m).
  x <- matrix(rep(1:3, times = 100), ncol = 3, byrow = TRUE)
  expect_equal(randolphKappa(x), 0, tolerance = 1e-10)
})

test_that("randolphKappa works for 2-rater case", {
  x <- matrix(c(1,1, 2,2, 1,2, 3,3), ncol=2, byrow=TRUE)
  res <- randolphKappa(x)
  expect_true(is.numeric(res))
})

test_that("randolphKappa is higher for more concentrated agreement", {
  x_strong <- matrix(c(1,1,1, 2,2,2, 1,1,1, 3,3,3), ncol=3, byrow=TRUE)
  x_weak   <- matrix(c(1,2,3, 1,2,3, 1,2,3, 1,2,3), ncol=3, byrow=TRUE)
  expect_gt(randolphKappa(x_strong), randolphKappa(x_weak))
})
