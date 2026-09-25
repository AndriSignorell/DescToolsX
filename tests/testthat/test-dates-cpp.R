# The compiled date helpers, called directly - isoYear_cpp() has no R
# caller at all, and the NA branches are only reached this way.

d <- as.Date(c("2019-12-30", "2021-01-03", "2020-12-31", "1969-12-29", NA))

test_that("ISO week, year and year-week agree with format()", {
  ok <- !is.na(d)
  expect_identical(isoWeek_cpp(d[ok]), as.integer(format(d[ok], "%V")))
  expect_identical(isoYear_cpp(d[ok]), as.integer(format(d[ok], "%G")))
  expect_identical(isoYearweek_cpp(d[ok]),
                   as.integer(format(d[ok], "%G%V")))
})

test_that("every helper passes NA through", {
  na <- as.Date(NA)
  for (f in list(isoWeek_cpp, usWeek_cpp, isoYear_cpp, isoYearweek_cpp,
                 usYearweek_cpp, usYearmonth_cpp))
    expect_identical(f(na), NA_integer_)
  expect_identical(isLeapYearDate_cpp(na), NA)
  expect_identical(isLeapYearInt_cpp(NA_integer_), NA)
})

test_that("the 'us' week counts seven-day blocks from 1 January", {
  # documents the current behaviour, which is neither %U nor %W; what
  # method = "us" should compute is still an open question
  x <- as.Date(c("2019-01-01", "2019-01-07", "2019-01-08", "2019-12-31"))
  expect_identical(usWeek_cpp(x), c(1L, 1L, 2L, 53L))
  expect_identical(usYearweek_cpp(x), c(201901L, 201901L, 201902L, 201953L))
})

test_that("year-month and leap years", {
  x <- as.Date(c("2024-02-29", "1900-03-01", "2000-12-31"))
  expect_identical(usYearmonth_cpp(x), c(202402L, 190003L, 200012L))
  expect_identical(isLeapYearDate_cpp(x), c(TRUE, FALSE, TRUE))
  expect_identical(isLeapYearInt_cpp(c(1900L, 2000L, 2023L, 2024L)),
                   c(FALSE, TRUE, FALSE, TRUE))
})
