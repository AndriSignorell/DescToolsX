test_that("addMonths adds one month to a normal date", {
  expect_equal(addMonths(as.Date("2023-03-15"), 1), as.Date("2023-04-15"))
})

test_that("addMonths subtracts months with negative n", {
  expect_equal(addMonths(as.Date("2023-03-15"), -1), as.Date("2023-02-15"))
})

test_that("addMonths clamps end-of-month: Jan 31 + 1 month = Feb 28", {
  expect_equal(addMonths(as.Date("2023-01-31"), 1), as.Date("2023-02-28"))
})

test_that("addMonths clamps end-of-month: Jan 31 + 1 month = Feb 29 (leap year)", {
  expect_equal(addMonths(as.Date("2024-01-31"), 1), as.Date("2024-02-29"))
})

test_that("addMonths handles March 31 - 1 month = Feb 28", {
  expect_equal(addMonths(as.Date("2023-03-31"), -1), as.Date("2023-02-28"))
})

test_that("addMonths crosses year boundary upward", {
  expect_equal(addMonths(as.Date("2022-11-15"), 3), as.Date("2023-02-15"))
})

test_that("addMonths crosses year boundary downward", {
  expect_equal(addMonths(as.Date("2023-02-15"), -3), as.Date("2022-11-15"))
})

test_that("addMonths is vectorised over x and n", {
  dates <- as.Date(c("2023-01-31", "2023-03-31"))
  result <- addMonths(dates, 1)
  expect_equal(result, as.Date(c("2023-02-28", "2023-04-30")))
})

test_that("addMonths recycles n along x", {
  dates <- as.Date(c("2023-01-15", "2023-03-15", "2023-05-15", "2023-07-15"))
  result <- addMonths(dates, c(1, -1))
  expect_equal(result,
               as.Date(c("2023-02-15", "2023-02-15", "2023-06-15", "2023-06-15")))
})

test_that("addMonths returns a Date object", {
  res <- addMonths(as.Date("2023-01-01"), 2)
  expect_s3_class(res, "Date")
})

test_that("addMonths coerces character input to Date", {
  expect_equal(addMonths("2023-01-31", 1), as.Date("2023-02-28"))
})

test_that("addMonths with n = 0 returns the same date", {
  d <- as.Date("2023-06-15")
  expect_equal(addMonths(d, 0), d)
})


# Additional branch coverage and reference checks
test_that("addMonths validates month counts and finite dates", {
  d <- as.Date("2024-01-31")
  expect_error(addMonths(d, "1"), "numeric vector")
  for (bad in c(Inf, -Inf, 0.5))
    expect_error(addMonths(d, bad), "finite whole numbers")
  expect_error(addMonths(d, .Machine$integer.max + 1), "supported range")
  expect_error(addMonths(structure(Inf, class = "Date"), 1), "finite dates")
})

test_that("addMonths propagates missing and empty inputs", {
  empty <- as.Date(character())
  expect_identical(addMonths(empty, 1), empty)
  expect_identical(addMonths(as.Date("2024-01-31"), numeric()), empty)
  expect_equal(addMonths(c("2024-01-31", NA, "2024-03-31"), c(1, 1, NA)),
               as.Date(c("2024-02-29", NA, NA)))
})

test_that("addMonths forwards date conversion arguments and partially recycles", {
  d <- as.Date("2024-01-31")
  expect_equal(addMonths(as.numeric(d), 1, origin = "1970-01-01"), as.Date("2024-02-29"))
  expect_equal(addMonths("31/01/2024", 1, format = "%d/%m/%Y"), as.Date("2024-02-29"))
  expect_silent(ans <- addMonths(rep(d, 3), c(1, -1)))
  expect_equal(ans, as.Date(c("2024-02-29", "2023-12-31", "2024-02-29")))
  expect_equal(addMonths(d, c(0, 1, 12)),
               as.Date(c("2024-01-31", "2024-02-29", "2025-01-31")))
})

test_that("ym month arithmetic covers both operand orders and subtraction", {
  d <- as.ym(c(202312, 202401))
  expect_equal(addMonths(d, 1), as.ym(c(202401, 202402)))
  expect_equal(d + 1, as.ym(c(202401, 202402)))
  expect_equal(1 + d, d + 1)
  expect_identical(+d, d)
  expect_equal(d - 1, as.ym(c(202311, 202312)))
  expect_true(all(is.na(addMonths(d, NA_real_))))
  expect_error(d + d, "cannot be added")
  expect_error(-d, "unary")
  expect_error(d - d, "expects a number of months")
  for (bad in list("1", 0.5, Inf))
    expect_error(addMonths(d, bad), "whole finite numbers")
  # Defensive S3 method guard, not reachable via ordinary dispatch.
  expect_error(DescToolsX:::`+.ym`(1, 2), "one operand")
})
