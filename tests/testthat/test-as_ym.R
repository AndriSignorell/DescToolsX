test_that("as.ym creates an object of class ym", {
  expect_s3_class(as.ym(202308), "ym")
})

test_that("as.ym stores the correct integer value", {
  expect_equal(as.integer(as.ym(202308)), 202308L)
})

test_that("as.ym returns NA for invalid month 0", {
  expect_true(is.na(as.ym(202300)))
})

test_that("as.ym returns NA for invalid month 13", {
  expect_true(is.na(as.ym(202313)))
})

test_that("as.ym returns NA for year below 1000", {
  expect_true(is.na(as.ym(99901)))
})

test_that("as.ym is vectorised", {
  result <- as.ym(c(202301, 202312))
  expect_equal(length(result), 2L)
  expect_s3_class(result, "ym")
})

test_that("as.Date.ym converts to the first of the month by default", {
  expect_equal(as.Date(as.ym(202308)), as.Date("2023-08-01"))
})

test_that("as.Date.ym respects the d argument", {
  expect_equal(as.Date(as.ym(202308), d = 15), as.Date("2023-08-15"))
})

test_that("addMonths.ym adds months correctly within a year", {
  expect_equal(addMonths(as.ym(202303), 3), 202306L)
})

test_that("addMonths.ym crosses a year boundary upward", {
  expect_equal(addMonths(as.ym(202311), 3), 202402L)
})

test_that("addMonths.ym subtracts months across a year boundary", {
  expect_equal(addMonths(as.ym(202301), -2), 202211L)
})

test_that("+.ym operator adds months", {
  expect_equal(as.ym(202301) + 5L, 202306L)
})

test_that("-.ym operator subtracts months", {
  expect_equal(as.ym(202306) - 5L, 202301L)
})
