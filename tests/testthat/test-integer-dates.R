
test_that("date functions accept Dates with integer storage", {

  # seq.Date() delegates to seq.int() for by = "days", which returns an
  # INTEGER vector whenever the endpoints are whole numbers. A Date is
  # only required to be numeric, not double, so every compiled routine
  # that reads REAL() has to coerce first.
  x <- seq(as.Date("2019-01-01"), as.Date("2019-01-24"), by = "days")
  expect_identical(typeof(x), "integer")

  y <- as.Date("2019-01-01") + seq(0, 23)
  expect_identical(typeof(y), "double")

  # the two must be indistinguishable to every entry point
  expect_equal(fm(x, fmt = "ddd"), fm(y, fmt = "ddd"))
  expect_equal(week(x),            week(y))
  expect_equal(week(x, "us"),      week(x = y, method = "us"))
  expect_equal(yearWeek(x),        yearWeek(y))
  expect_equal(yearMonth(x),       yearMonth(y))
  expect_equal(isLeapYear(x),      isLeapYear(y))
  expect_equal(weekday(x),         weekday(y))
  expect_equal(yearDay(x),         yearDay(y))
})


test_that("countWorkDays accepts both storage modes", {

  a <- seq(as.Date("2019-01-01"), as.Date("2019-01-24"), by = "days")
  b <- as.Date("2019-01-01") + seq(0, 23)

  expect_equal(countWorkDays(min(a), max(a)), countWorkDays(min(b), max(b)))
})
