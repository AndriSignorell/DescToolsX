test_that("hmsToSec parses scalar, vector and factor inputs", {
  expect_equal(unname(hmsToSec("02:00:03")), 7203)
  expect_equal(unname(hmsToSec(c("00:00:00", "01:02:03", "25:00:00"))), c(0, 3723, 90000))
  expect_equal(unname(hmsToSec(factor(c("00:00:30", "01:00:00")))), c(30, 3600))
  expect_equal(unname(hmsToSec("00:00:01.125")), 1.125)
})

test_that("secToHms handles integer and explicit fractional precision", {
  expect_identical(secToHms(c(0, 59, 60, 3600, 90061)),
                   c("00:00:00", "00:00:59", "00:01:00", "01:00:00", "25:01:01"))
  expect_identical(secToHms(1.25), "00:00:01.25")
  expect_identical(secToHms(1.125, digits = 3), "00:00:01.125")
  expect_identical(secToHms(1.25, digits = 0), "00:00:01")
  expect_identical(secToHms(c(1, 1.5), digits = 2), c("00:00:01.00", "00:00:01.50"))
  x <- c(0, 12.125, 3723.5, 90000)
  expect_equal(unname(hmsToSec(secToHms(x, digits = 3))), x)
})

test_that("hmsToMinute uses hour, minute and fractional seconds", {
  x <- as.POSIXct(c("2024-01-01 00:00:00", "2024-01-01 01:02:30"), tz = "UTC")
  expect_equal(hmsToMinute(x), c(0, 62.5))
  expect_equal(hmsToMinute(as.POSIXlt(x, tz = "UTC")), c(0, 62.5))
})


test_that("secToHms propagates rounded fractions through seconds, minutes and hours", {
  expect_identical(secToHms(c(1.999, 59.999, 3599.999, 86399.999), digits = 2),
                   c("00:00:02.00", "00:01:00.00", "01:00:00.00", "24:00:00.00"))
  expect_identical(secToHms(59.999), "00:01:00.00")
  expect_identical(secToHms(c(59.994, 59.996), digits = 2),
                   c("00:00:59.99", "00:01:00.00"))
  expect_identical(secToHms(59.9999, digits = 3), "00:01:00.000")
})

test_that("secToHms retains zero-digit truncation and negative floor decomposition", {
  expect_identical(secToHms(c(1.9, 59.999, -0.1, -1), digits = 0),
                   c("00:00:01", "00:00:59", "-1:59:59", "-1:59:59"))
  expect_identical(secToHms(-0.001, digits = 2), "00:00:00.00")
})

test_that("secToHms handles missing and empty inputs and validates precision", {
  expect_identical(secToHms(numeric()), character())
  expect_identical(secToHms(c(NA_real_, NaN)), c(NA_character_, NA_character_))
  expect_identical(secToHms(c(NA_real_, 1.25)), c(NA_character_, "00:00:01.25"))
  expect_identical(secToHms(c(NA_real_, 1)), c(NA_character_, "00:00:01"))
  expect_error(secToHms(Inf), "finite seconds")
  expect_error(secToHms(-Inf), "finite seconds")
  for (bad in list("2", numeric(), c(1, 2), NA_real_, Inf, -1, 1.5, 16))
    expect_error(secToHms(1, digits = bad), "'digits'")
  expect_identical(secToHms(1.125, digits = 15), "00:00:01.125000000000000")
})
