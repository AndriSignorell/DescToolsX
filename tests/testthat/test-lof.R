
skip_if_not_installed("dbscan")


test_that("lof returns a numeric vector of length nrow(x)", {

  set.seed(1)
  x <- matrix(rnorm(40), ncol = 2)
  res <- lof(x, k = 3)

  expect_type(res, "double")
  expect_length(res, nrow(x))
  expect_false(anyNA(res))

})


test_that("lof scores are positive", {

  set.seed(2)
  x <- matrix(rnorm(60), ncol = 2)

  expect_true(all(lof(x, k = 5) > 0))

})


test_that("lof identifies an obvious outlier with a high score", {

  set.seed(3)
  x <- rbind(matrix(rnorm(40, sd = 0.1), ncol = 2),
             matrix(c(100, 100), ncol = 2))

  scores <- lof(x, k = 5)
  outlier <- nrow(x)

  expect_gt(scores[outlier], max(scores[-outlier]))

  # and by a wide margin, not merely by a hair
  expect_gt(scores[outlier], 2)

})


test_that("lof scores for a homogeneous cloud stay near 1", {

  set.seed(4)
  x <- matrix(rnorm(100), ncol = 2)

  res <- lof(x, k = 5)

  # interior points sit near 1; a few boundary points deviate, so the
  # median is the meaningful summary rather than every value
  expect_lt(median(abs(res - 1)), 0.3)

})


test_that("lof is invariant to scaling and translation of the data", {

  set.seed(6)
  x <- matrix(rnorm(60), ncol = 2)

  base <- lof(x, k = 5)

  # the LOF is a ratio of densities, so a common factor or offset cancels
  expect_equal(lof(x * 100, k = 5), base, tolerance = 1e-8)
  expect_equal(lof(x + 50, k = 5), base, tolerance = 1e-8)

})


test_that("lof accepts a data frame and matches the matrix result", {

  set.seed(5)
  df <- data.frame(x = rnorm(30), y = rnorm(30))

  res <- lof(df, k = 3)

  expect_length(res, 30)
  expect_equal(res, lof(as.matrix(df), k = 3))

})


test_that("lof handles duplicated observations without failing", {

  set.seed(7)
  # More than k copies of one point: every distance within the block is
  # zero, the local reachability density is infinite and the LOF is
  # formally 0/0. dbscan reports 1 for those points.
  #
  # The block is placed well away from the rest of the data on purpose.
  # An infinite lrd propagates to any point that has a block member among
  # its own neighbours, so a block sitting inside the cloud would make
  # surrounding points infinite too - a property of the definition, not a
  # fault of the implementation.
  x <- rbind(matrix(rep(0, 16), ncol = 2),
             matrix(rnorm(40), ncol = 2) + 50)

  res <- lof(x, k = 3)

  expect_equal(unname(res[1:8]), rep(1, 8))
  expect_true(all(is.finite(res[9:nrow(x)])))
  expect_false(anyNA(res))

})


test_that("lof propagates infinite density from an embedded duplicate block", {

  set.seed(11)
  # Duplicates sitting inside the cloud rather than apart from it: points
  # whose neighbourhood reaches into the block pick up its infinite lrd.
  # Pinned here so the behaviour is a known property and not a surprise.
  x <- rbind(matrix(rep(0, 16), ncol = 2),
             matrix(rnorm(40), ncol = 2))

  res <- lof(x, k = 3)

  expect_equal(unname(res[1:8]), rep(1, 8))
  expect_true(any(is.infinite(res[9:nrow(x)])))

})


test_that("lof rejects k that does not fit the data", {

  set.seed(8)
  x <- matrix(rnorm(20), ncol = 2)

  # k neighbours plus the point itself need k + 1 observations
  expect_error(lof(x, k = nrow(x)), "smaller than the number")
  expect_error(lof(x, k = nrow(x) + 5), "smaller than the number")

})


test_that("lof validates its arguments", {

  set.seed(9)
  x <- matrix(rnorm(40), ncol = 2)

  expect_error(lof(x, k = 0), "positive whole number")
  expect_error(lof(x, k = 1.5), "positive whole number")
  expect_error(lof(x, k = c(2, 3)), "positive whole number")
  expect_error(lof(x, k = NA), "positive whole number")

  expect_error(lof("a", k = 3), "matrix or a data frame")
  expect_error(lof(data.frame(a = letters[1:5]), k = 2), "must be numeric")

})


test_that("lof rejects missing and infinite values", {

  x <- matrix(c(1, 2, 3, 4, 5, NA, 7, 8, 9, 10), ncol = 2)
  expect_error(lof(x, k = 2), "missing")

  y <- matrix(c(1, 2, 3, 4, 5, Inf, 7, 8, 9, 10), ncol = 2)
  expect_error(lof(y, k = 2), "infinite")

})


test_that("lof k translates to dbscan minPts correctly", {

  set.seed(10)
  x <- matrix(rnorm(60), ncol = 2)

  # k counts neighbours only, dbscan's minPts includes the point itself
  expect_equal(lof(x, k = 5), dbscan::lof(x, minPts = 6))

})
