d <- as.Date(c("2024-08-11", "2024-01-01", "2023-12-31"))   # Sun, Mon, Sun

test_that("month() and weekday() give numbers, abbreviations and names", {
  expect_equal(month(d), c(8, 1, 12))
  expect_identical(as.character(month(d, fmt = "mm", lang = "en")),
                   c("Aug", "Jan", "Dec"))
  expect_identical(month(d, fmt = "mmm", lang = "en", stringsAsFactors = FALSE),
                   c("August", "January", "December"))
  expect_true(is.ordered(month(d, fmt = "mm", lang = "en")))
  expect_identical(levels(month(d, fmt = "mm", lang = "local")),
                   format(ISOdate(2000, 1:12, 1), "%b"))
  expect_equal(weekday(d), c(7, 1, 7))                # ISO: Sunday is 7
  expect_identical(as.character(weekday(d, fmt = "dd", lang = "en")),
                   c("Sun", "Mon", "Sun"))
  expect_identical(weekday(d, fmt = "ddd", lang = "en", stringsAsFactors = FALSE),
                   c("Sunday", "Monday", "Sunday"))
  expect_identical(levels(weekday(d, fmt = "ddd", lang = "local")),
                   format(ISOdate(2000, 1, 3:9), "%A"))
  expect_equal(month(as.ym(202408)), 8)
})

test_that("the simple extractors", {
  t <- as.POSIXct("2024-08-11 14:25:36", tz = "UTC")
  expect_equal(year(d), c(2024, 2024, 2023))
  expect_equal(quarter(d), c(3, 1, 4))
  expect_equal(yearDay(d), c(224, 1, 365))
  expect_equal(c(hour(t), minute(t), second(t)), c(14, 25, 36))
  expect_identical(timezone(t), "UTC")
  expect_equal(isWeekend(d), c(TRUE, FALSE, TRUE))
  expect_s3_class(today(), "Date")
  expect_s3_class(now(), "POSIXct")
})

test_that("yearMonth, week and yearWeek", {
  expect_equal(yearMonth(d), c(202408, 202401, 202312))
  expect_equal(week(d), as.integer(format(d, "%V")))
  expect_equal(yearWeek(d), as.integer(format(d, "%G%V")))
  # the US variants are exercised, not asserted: what "us" is meant to
  # compute is an open question
  expect_length(week(d, method = "us"), 3L)
  expect_length(yearWeek(d, method = "us"), 3L)
})

test_that("lastDayOfMonth handles month lengths and leap years", {
  expect_equal(lastDayOfMonth(as.Date(c("2024-02-10", "2023-02-10", "2024-04-30"))),
               as.Date(c("2024-02-29", "2023-02-28", "2024-04-30")))
})

test_that("diffDays360: European and US 30/360", {
  s <- as.Date(c("2024-01-31", "2024-01-30", "2023-02-28", "2023-02-28"))
  e <- as.Date(c("2024-03-31", "2024-03-31", "2023-03-31", "2024-02-29"))
  expect_equal(diffDays360(s, e, "eu"), c(60, 60, 32, 361))
  # US: day 31 of the end date only counts as 30 if the start was the
  # 30th/31st or the end of February; two month-ends of February pair up
  expect_equal(diffDays360(s, e, "us"), c(60, 61, 30, 360))
})

test_that("isLeapYear accepts whole years and dates", {
  expect_identical(isLeapYear(c(1900, 2000, 2023, 2024)),
                   c(FALSE, TRUE, FALSE, TRUE))
  expect_identical(isLeapYear(as.Date("2024-05-01")), TRUE)
  expect_error(isLeapYear(2024.5), "whole years")
})

test_that("day<- keeps the clock time across a daylight-saving change", {
  # 15 March to 31 March crosses the switch to CEST on 31 March 2024; adding
  # 16 * 86400 s gave 13:00
  x <- as.POSIXct("2024-03-15 12:00:00", tz = "Europe/Zurich")
  day(x) <- 31
  expect_equal(format(x, "%Y-%m-%d %H:%M"), "2024-03-31 12:00")
  expect_identical(attr(x, "tzone"), "Europe/Zurich")
  lt <- as.POSIXlt("2024-01-10 08:30:00", tz = "UTC")
  day(lt) <- 12
  expect_s3_class(lt, "POSIXlt")
  expect_equal(lt$mday, 12)
})

test_that(".asDateInTz converts characters and POSIXlt", {
  expect_equal(.asDateInTz("2024-02-03"), as.Date("2024-02-03"))
  lt <- as.POSIXlt("2019-01-01 00:30", tz = "Europe/Zurich")
  expect_equal(.asDateInTz(lt), as.Date("2019-01-01"))
})

test_that("hasVaryingTime validates na.rm and handles missing values", {
  t <- as.POSIXct(c("2024-01-01 08:00", "2024-01-02 09:00", NA), tz = "UTC")
  expect_error(hasVaryingTime(t, na.rm = NA), "na.rm")
  expect_true(is.na(hasVaryingTime(t)))
  expect_true(hasVaryingTime(t, na.rm = TRUE))
  expect_false(hasVaryingTime(t[1]))
})

test_that("chron classes are recognised", {
  # The predicates look only at the class and at the numeric value, so
  # objects carrying chron's classes are enough - no dependency on chron,
  # which would otherwise have to be declared in Suggests for R CMD check.
  # chron stores days since 1970 with the time of day as the fraction.
  chronLike <- function(v, cls) structure(v, class = cls)
  dd <- chronLike(19723, c("dates", "times"))                  # 2024-01-01
  tt <- chronLike(8 / 24, "times")                             # 08:00
  ch <- chronLike(19723 + c(8, 9.5) / 24, c("chron", "dates", "times"))

  expect_true(isDate(dd));  expect_false(isTime(dd))
  expect_false(isDate(tt)); expect_true(isTime(tt))
  expect_true(isDateTime(ch))
  expect_true(hasVaryingTime(ch))
  expect_false(hasVaryingTime(chronLike(c(19723, 19724) + 8 / 24,
                                        c("chron", "dates", "times"))))
})

test_that("isoYear is the year of the ISO week", {
  x <- as.Date(c("2019-12-30", "2021-01-03", "2024-06-15", "1969-12-29", NA))
  expect_identical(isoYear(x), c(2020L, 2020L, 2024L, 1970L, NA))
  expect_identical(isoYear(x[1:4]), as.integer(format(x[1:4], "%G")))
  # a date-time counts in its own zone, like week()
  t <- as.POSIXct("2019-12-29 23:30", tz = "America/New_York")  # Sunday
  expect_identical(isoYear(t), 2019L)
  expect_identical(week(t), 52L)
})
