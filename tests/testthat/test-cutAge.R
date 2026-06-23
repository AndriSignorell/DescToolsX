test_that("cutAge returns a factor", {
  x <- sample(0:80, 100, replace = TRUE)
  expect_s3_class(cutAge(x), "factor")
})

test_that("cutAge default produces an ordered factor", {
  x <- sample(0:80, 100, replace = TRUE)
  expect_true(is.ordered(cutAge(x)))
})

test_that("cutAge default intervals are right = FALSE (left-closed)", {
  # Age 10 should fall in [10, 20), not [0, 10)
  x <- c(0L, 10L, 20L)
  res <- cutAge(x)
  lvls <- levels(res)
  # The label containing 10 should be the second group
  expect_equal(as.integer(res[x == 10L]), 2L)
})

test_that("cutAge values outside breaks are coded as NA", {
  x <- c(-1L, 50L, 200L)
  res <- cutAge(x)
  # -1 is below 0 and 200 is above Inf → only Inf captures 200
  # -1 should be NA
  expect_true(is.na(res[1]))
})

test_that("cutAge total non-NA count equals number of in-range values", {
  x   <- c(sample(0:89, 95, replace = TRUE), rep(-5L, 5))
  res <- cutAge(x)
  expect_equal(sum(!is.na(res)), 95L)
})

test_that("cutAge custom breaks are respected", {
  x   <- 0:100
  res <- cutAge(x, breaks = c(0, 18, 65, Inf))
  expect_equal(nlevels(res), 3L)
})

test_that("cutAge labels = TRUE generates age-range labels with a dash separator", {
  x   <- c(5L, 15L, 25L)
  res <- cutAge(x, labels = TRUE)
  # Labels should contain a '-' separator; fm() formatting may or may not zero-pad
  expect_true(all(grepl("-", levels(res))))
})

test_that("cutAge full = FALSE drops empty edge levels", {
  x   <- c(20L, 30L, 40L)   # only 20-50 range occupied
  res_full  <- cutAge(x, full = TRUE)
  res_trim  <- cutAge(x, full = FALSE)
  expect_gte(nlevels(res_full), nlevels(res_trim))
})

test_that("cutAge orderedResult = FALSE returns an unordered factor", {
  x <- sample(0:80, 50, replace = TRUE)
  res <- cutAge(x, orderedResult = FALSE)
  expect_false(is.ordered(res))
})
