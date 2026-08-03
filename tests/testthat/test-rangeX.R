
test_that("rangeX() returns the span and its bounds", {

  x <- c(0:10, 50)
  r <- rangeX(x)
  expect_equal(as.vector(r), 50)
  expect_equal(attr(r, "bounds"), c(0, 50))
})


test_that("rangeX() treats na.rm the same way in both branches", {

  set.seed(3)
  x <- c(rnorm(30), NA)

  # the robust branch dropped NAs unconditionally via x[is.finite(x)] while
  # the conventional branch returned NA
  expect_true(is.na(rangeX(x)))
  expect_true(is.na(rangeX(x, robust = TRUE)))

  expect_false(is.na(rangeX(x, na.rm = TRUE)))
  expect_false(is.na(rangeX(x, robust = TRUE, na.rm = TRUE)))
})


test_that("rangeX(robust = TRUE) shrinks the span of a contaminated sample", {

  set.seed(7)
  x <- c(rnorm(40), 100, -100)
  expect_lt(as.vector(rangeX(x, robust = TRUE)), as.vector(rangeX(x)))
  expect_length(attr(rangeX(x, robust = TRUE), "bounds"), 2L)
})


test_that("rangeX() falls back to the ordinary range on degenerate input", {

  expect_warning(r <- rangeX(c(1, 1, 1, 1, 1), robust = TRUE),
                 "width 0")
  expect_equal(attr(r, "bounds"), c(1, 1))

  expect_warning(r2 <- rangeX(c(1, 2), robust = TRUE), "Not enough valid data")
  expect_equal(attr(r2, "bounds"), c(1, 2))
})
