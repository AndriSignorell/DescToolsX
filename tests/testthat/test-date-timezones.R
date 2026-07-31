
# The invariant throughout: a date-time's calendar day is the one its own
# time zone shows, which is what format() reports. Asserting agreement
# with format() rather than a literal value keeps these tests independent
# of the machine's zone and of what its C runtime makes of TZ.

test_that("week/yearWeek/yearMonth follow the timestamp's own zone", {

  # 00:30 in a positive-offset zone is the PREVIOUS day in UTC, and
  # as.Date.POSIXct() defaults to tz = "UTC" since R 4.3
  x <- as.POSIXct("2019-01-01 00:30:00", tz = "Europe/Zurich")

  expect_equal(as.Date(x), as.Date("2018-12-31"))          # the trap
  expect_equal(format(x, "%Y-%m-%d"), "2019-01-01")        # the truth

  d <- as.Date("2019-01-01")

  expect_equal(week(x), week(d))
  expect_equal(week(x, method = "us"), week(d, method = "us"))
  expect_equal(unclass(yearWeek(x)), unclass(yearWeek(d)))
  expect_equal(unclass(yearMonth(x)), unclass(yearMonth(d)))
})


test_that("the year boundary is not crossed by the coercion", {

  # 2020-01-01 00:30 CET is 2019-12-31 23:30 UTC: a naive as.Date() moves
  # the value into the previous YEAR, not just the previous day
  x <- as.POSIXct("2020-01-01 00:30:00", tz = "Europe/Zurich")

  expect_equal(unclass(yearMonth(x)), unclass(yearMonth(as.Date("2020-01-01"))))
  expect_true(isLeapYear(x))                     # 2020, not 2019
  expect_equal(isLeapYear(x), isLeapYear(as.Date("2020-01-01")))
})


test_that("a negative-offset zone is handled too", {

  # 23:30 in New York is already the NEXT day in UTC
  x <- as.POSIXct("2019-06-30 23:30:00", tz = "America/New_York")

  expect_equal(format(x, "%Y-%m-%d"), "2019-06-30")
  expect_equal(unclass(yearMonth(x)),
               unclass(yearMonth(as.Date("2019-06-30"))))
  expect_equal(week(x), week(as.Date("2019-06-30")))
})


test_that("the POSIXlt-based extractors were already correct", {

  # as.POSIXlt() breaks a Date down in UTC and a POSIXct in its own zone,
  # so these need no coercion - adding one would introduce the very bug
  # the .Call sites had.
  x <- as.POSIXct("2019-01-01 00:30:00", tz = "Europe/Zurich")

  expect_equal(year(x), 2019L)
  expect_equal(month(x), 1L)
  expect_equal(day(x), 1L)
  expect_equal(quarter(x), 1L)
  expect_equal(yearDay(x), 1L)
  expect_equal(weekday(x), as.POSIXlt(x)$wday)
  expect_equal(hour(x), 0L)
  expect_equal(minute(x), 30L)
})


test_that("Dates and POSIXlt go through unchanged", {

  d <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by = "days")

  expect_equal(week(d), week(as.POSIXct(paste(d, "12:00:00"), tz = "UTC")))

  lt <- as.POSIXlt("2019-07-01 14:45:00", tz = "Europe/Zurich")
  expect_equal(unclass(yearMonth(lt)),
               unclass(yearMonth(as.Date("2019-07-01"))))
})


test_that("a timestamp without tzone uses the session zone, like format()", {

  x <- as.POSIXct("2019-03-15 08:00:00")     # tzone is ""

  expect_equal(unclass(yearMonth(x)),
               unclass(yearMonth(as.Date(format(x, "%Y-%m-%d")))))
})


test_that("missing dates come back as NA, not as a number", {

  # usYearmonth() and isLeapYearDate() had no NA guard, unlike every
  # other routine in dates.cpp
  d <- as.Date(c("2020-03-15", NA))

  expect_equal(unclass(yearMonth(d)), c(202003L, NA_integer_))
  expect_equal(isLeapYear(d), c(TRUE, NA))

  expect_equal(week(d), c(week(as.Date("2020-03-15")), NA_integer_))
  expect_equal(unclass(yearWeek(d)),
               c(unclass(yearWeek(as.Date("2020-03-15"))), NA_integer_))

  # NA_INTEGER is INT_MIN; INT_MIN %% 4 == 0 and %% 100 == -48, so the
  # integer branch used to report a missing year as a leap year
  expect_equal(isLeapYear(c(2020L, NA_integer_)), c(TRUE, NA))
})


test_that("isLeapYear returns a logical, not 0/1", {

  expect_type(isLeapYear(2020L), "logical")
  expect_type(isLeapYear(as.Date("2020-01-01")), "logical")

  expect_true(isLeapYear(2000L))
  expect_false(isLeapYear(1900L))
  expect_true(isLeapYear(as.Date("2024-06-01")))
})


test_that("ISO weeks and years match format() at the year boundaries", {

  # the cases where the ISO year differs from the calendar year
  d <- as.Date(c("2019-12-30", "2021-01-01", "2021-01-04",
                 "2016-01-01", "2011-01-01", "2015-12-31"))

  expect_equal(week(d), as.integer(format(d, "%V")))
  expect_equal(unclass(yearWeek(d)),
               as.integer(format(d, "%G")) * 100L + as.integer(format(d, "%V")))
})


test_that("ISO weeks are right for dates before 1970", {

  # (days + 4) %% 7 is negative there in C++, which the old code survived
  # only because the following (wday + 6) %% 7 cancelled the shift
  d <- seq(as.Date("1965-01-01"), as.Date("1969-12-31"), by = "days")

  expect_equal(week(d), as.integer(format(d, "%V")))
})
