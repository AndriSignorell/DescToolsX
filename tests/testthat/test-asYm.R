test_that("asYm creates an object of class ym", {
  expect_s3_class(asYm(202308), "ym")
})

test_that("asYm stores the correct integer value", {
  expect_equal(as.integer(asYm(202308)), 202308L)
})

test_that("asYm returns NA for invalid month 0", {
  expect_true(is.na(asYm(202300)))
})

test_that("asYm returns NA for invalid month 13", {
  expect_true(is.na(asYm(202313)))
})

test_that("asYm returns NA for year below 1000", {
  expect_true(is.na(asYm(99901)))
})

test_that("asYm is vectorised", {
  result <- asYm(c(202301, 202312))
  expect_equal(length(result), 2L)
  expect_s3_class(result, "ym")
})

test_that("as.Date.ym converts to the first of the month by default", {
  expect_equal(as.Date(asYm(202308)), as.Date("2023-08-01"))
})

test_that("as.Date.ym respects the d argument", {
  expect_equal(as.Date(asYm(202308), d = 15), as.Date("2023-08-15"))
})

test_that("addMonths.ym adds months correctly within a year", {
  expect_equal(addMonths(asYm(202303), 3), 202306L)
})

test_that("addMonths.ym crosses a year boundary upward", {
  expect_equal(addMonths(asYm(202311), 3), 202402L)
})

test_that("addMonths.ym subtracts months across a year boundary", {
  expect_equal(addMonths(asYm(202301), -2), 202211L)
})

test_that("+.ym operator adds months", {
  expect_equal(asYm(202301) + 5L, 202306L)
})

test_that("-.ym operator subtracts months", {
  expect_equal(asYm(202306) - 5L, 202301L)
})


# Review 25.09.2026 ------------------------------------------------------------

test_that("dates and date-times are converted to their year and month", {
  expect_equal(unclass(asYm(as.Date(c("2023-08-15", NA)))), c(202308L, NA))
  # a date-time in its own zone: 00:30 on 1 January in Zurich is still 2019
  t <- as.POSIXct("2019-01-01 00:30", tz = "Europe/Zurich")
  expect_equal(unclass(asYm(t)), 201901L)
})

test_that("asYm accepts character and factor input and keeps names", {
  expect_equal(unclass(asYm(c(a = "202308", b = "x"))),
               c(a = 202308L, b = NA))
  expect_equal(unclass(asYm(factor("202401"))), 202401L)
  expect_true(is.na(asYm(202308.5)))
})

test_that("as.Date.ym validates d and gives NA for impossible days", {
  expect_true(is.na(as.Date(asYm(202302), d = 30)))
  expect_equal(as.Date(asYm(c(202301, NA))), as.Date(c("2023-01-01", NA)))
  for (d in list(0, 32, 1.5, NA, c(1, 2), "1"))
    expect_error(as.Date(asYm(202301), d = d), "'d'")
})

test_that("print.ym prints the bare integers and returns invisibly", {
  x <- asYm(c(202301, 202302))
  expect_output(res <- print(x), "202301 202302")
  expect_identical(res, x)
})
