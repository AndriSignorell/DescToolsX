withr::local_options(list(DescToolsX.plotit = FALSE))

x <- as.Date("2024-01-01") + 0:13     # two full weeks, starting on a Monday

test_that("desc.Date() computes the time-axis core", {
  d <- desc(x)
  expect_s3_class(d, c("Desc.Date", "Desc"))
  expect_identical(d$core$n, 14L)
  expect_identical(d$core$nMissing, 0L)
  expect_equal(d$core$min, x[1])
  expect_equal(d$core$max, x[14])
  expect_equal(d$core$span, 13)
  expect_identical(d$core$uniqueDays, 14L)
  expect_equal(d$core$coverage, 1)
  expect_equal(unname(d$core$quantiles["median"]),
               as.Date(median(as.numeric(x)), origin = "1970-01-01"))
  expect_equal(d$core$iqrDays,
               as.numeric(diff(quantile(as.numeric(x), c(.25, .75)))))
})

test_that("weekday and month distributions are tabulated", {
  d <- desc(x)
  expect_identical(names(d$weekday$observed),
                   weekdays(as.Date("2023-01-02") + 0:6))
  expect_equal(unname(d$weekday$observed), rep(2, 7))
  expect_equal(d$weekday$p.value, 1)
  expect_equal(unname(d$month$observed), c(14, rep(0, 11)))
  # default mprobs = NULL: expected counts proportional to month length
  expect_equal(unname(d$month$expected[1]), 14 * 31 / 365)
  expect_equal(unname(d$month$expected[2]), 14 * 28 / 365)
})

test_that("weekdays do not depend on the session time zone", {
  mondays <- as.Date("2024-01-01") + 7 * (0:3)
  for (tz in c("America/New_York", "Pacific/Kiritimati", "UTC")) {
    withr::local_timezone(tz)
    expect_equal(unname(desc(mondays)$weekday$observed),
                 c(4, rep(0, 6)), info = tz)
  }
})

test_that("wprobs and mprobs define the expected counts", {
  d <- desc(x, wprobs = c(rep(1, 5), 0.5, 0.5))
  expect_equal(unname(d$weekday$expected[1]), 14 / 6)
  dm <- desc(x, mprobs = rep(1, 12))    # uniform on request
  expect_equal(unname(dm$month$expected[1]), 14 / 12)
  expect_error(desc(x, wprobs = rep(1, 6)), "length 7")
  expect_error(desc(x, mprobs = rep(1, 11)), "length 12")
})

test_that("missing values are counted, not described", {
  d <- desc(c(x, NA))
  expect_identical(d$core$length, 15L)
  expect_identical(d$core$nMissing, 1L)
  expect_identical(d$core$n, 14L)
})

test_that("sentinel detection collects every triggered reason", {
  expect_false(desc(x)$sentinel$flag)
  expect_length(desc(x)$sentinel$reason, 0L)
  s <- as.Date(c("1899-06-30", "2020-01-01", "9999-12-31"))
  ds <- desc(s)
  expect_true(ds$sentinel$flag)
  expect_length(ds$sentinel$reason, 3L)
  expect_output(print(ds), "Sentinel detected")
})

test_that("print.Desc.Date shows blocks according to verbose", {
  d <- desc(x)
  out1 <- capture.output(res <- print(d, verbose = 1))
  expect_identical(res, d)
  expect_true(any(grepl("median", out1)))
  expect_false(any(grepl("Weekday distribution", out1)))
  out2 <- capture.output(print(d, verbose = 2))
  expect_true(any(grepl("Weekday distribution", out2)))
  expect_false(any(grepl("Month distribution", out2)))
  out3 <- capture.output(print(d, verbose = 3))
  expect_true(any(grepl("Month distribution", out3)))
})

test_that(".residMark() maps residuals to the documented markers", {
  expect_identical(
    as.character(.residMark(c(-4, -2.5, -1.7, 0, 1.7, 2.5, 4))),
    c("---", "--", "-", "", "+", "++", "+++"))
})
