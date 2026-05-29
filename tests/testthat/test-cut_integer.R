test_that("cut.integer returns a factor", {
  x <- as.integer(c(1, 5, 10, 15, 20))
  res <- cut(x, breaks = c(0L, 10L, 20L))
  expect_s3_class(res, "factor")
})

test_that("cut.integer default labels use integer-friendly 'a-b' format", {
  x <- as.integer(c(1, 5, 10, 15, 20))
  res <- cut(x, breaks = c(0L, 10L, 20L))
  # Labels should be "1-10" and "11-20", not "(0,10]" etc.
  expect_match(levels(res)[1], "^[0-9]+-[0-9]+$")
})

test_that("cut.integer assigns all values to a level", {
  x <- as.integer(c(1, 5, 8, 12, 18))
  res <- cut(x, breaks = c(0L, 10L, 20L))
  expect_false(any(is.na(res)))
})

test_that("cut.integer number of levels equals length(breaks) - 1", {
  x <- as.integer(1:20)
  res <- cut(x, breaks = c(0L, 5L, 10L, 20L))
  expect_equal(nlevels(res), 3L)
})

test_that("cut.integer custom labels are respected", {
  x  <- as.integer(c(1, 5, 15))
  res <- cut(x, breaks = c(0L, 10L, 20L), labels = c("low","high"))
  expect_equal(levels(res), c("low","high"))
})

test_that("cut.integer labels = FALSE returns integer codes", {
  x  <- as.integer(c(1, 5, 15))
  res <- cut(x, breaks = c(0L, 10L, 20L), labels = FALSE)
  expect_type(res, "integer")
})

test_that("cut.integer ordered_result = TRUE returns an ordered factor", {
  x  <- as.integer(c(1, 5, 15))
  res <- cut(x, breaks = c(0L, 10L, 20L), ordered_result = TRUE)
  expect_true(is.ordered(res))
})

test_that("cut.integer right = FALSE uses left-closed intervals", {
  x   <- as.integer(c(0, 5, 10, 15))
  res <- cut(x, breaks = c(0L, 10L, 20L), right = FALSE, include.lowest = TRUE)
  # x = 10 should go to the second interval [10, 20)
  expect_equal(as.integer(res[x == 10L]), 2L)
})

test_that("cut.integer handles Inf in breaks gracefully", {
  x  <- as.integer(c(1, 50, 200))
  res <- cut(x, breaks = c(0L, 10L, .Machine$integer.max))
  expect_equal(nlevels(res), 2L)
})
