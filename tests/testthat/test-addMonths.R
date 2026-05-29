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
