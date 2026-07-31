
test_that("diffDays360 actually applies the European 30/360 rule", {

  # the documented example cannot distinguish the conventions: both ends
  # are the 31st, so eu and us agree at 60
  expect_equal(diffDays360(as.Date("2023-01-31"), as.Date("2023-03-31")), 60)

  # this one can. eu: 31 -> 30, so 30 + (28 - 30) = 28.
  # Without the adjustment the result was 27.
  expect_equal(diffDays360(as.Date("2023-01-31"), as.Date("2023-02-28")), 28)

  # a whole year is 360 days under both conventions
  expect_equal(diffDays360(as.Date("2023-01-01"), as.Date("2024-01-01")), 360)
  expect_equal(diffDays360(as.Date("2023-01-01"), as.Date("2024-01-01"),
                           method = "us"), 360)
})


test_that("diffDays360 is vectorised", {

  from <- as.Date(c("2023-01-31", "2023-01-01"))
  to   <- as.Date(c("2023-02-28", "2024-01-01"))

  expect_equal(diffDays360(from, to), c(28, 360))
})


test_that("yearMonth and isLeapYear cope with date-times", {

  d  <- as.Date("2024-03-15")
  dt <- as.POSIXct("2024-03-15 13:45:00", tz = "UTC")

  # the compiled routines read days since the epoch; a POSIXct counts
  # seconds and used to land some 86400 times too far in the future
  expect_equal(unclass(yearMonth(dt)), unclass(yearMonth(d)))
  expect_equal(isLeapYear(dt), isLeapYear(d))

  expect_true(isLeapYear(2000L))
  expect_false(isLeapYear(1900L))
  expect_true(isLeapYear(as.Date("2024-01-01")))
})


test_that("day<- moves a POSIXct to the requested day of the month", {

  x <- as.POSIXct("2024-01-10 08:30:00", tz = "UTC")
  day(x) <- 20

  expect_equal(day(x), 20)
  expect_equal(hour(x), 8)   # "+" adds seconds to a POSIXct
  expect_equal(minute(x), 30)

  d <- as.Date("2024-01-10")
  day(d) <- 20
  expect_equal(d, as.Date("2024-01-20"))
})


test_that("yearDays and monthDays stay vectorised", {

  x <- as.Date(c("2024-02-05", "2023-02-05", "2024-12-31"))

  expect_equal(monthDays(x), c(29, 28, 31))
  expect_equal(yearDays(x), c(366, 365, 366))
})


test_that("entropy tabulates a categorical vector", {

  x <- c("A", "A", "B", "B", "C")

  # as.numeric() on a character vector used to give NA with a warning
  expect_false(is.na(entropy(x)))
  expect_equal(entropy(x), entropy(c(2, 2, 1)))

  # a fair coin is exactly one bit, a fair die log2(6)
  expect_equal(entropy(c(50, 50)), 1)
  expect_equal(entropy(rep(1, 6)), log2(6))
  expect_equal(entropy(rep(1, 6), base = exp(1)), log(6))

  # maximum entropy normalizes to 1, a single category to 0
  expect_equal(entropy(rep(1, 6), normalize = TRUE), 1)
  expect_equal(entropy(c(5, 0, 0), normalize = TRUE), 0)
})


test_that("cutAge keeps the factor ordered when trimming empty levels", {

  x <- c(42, 47, 51)

  full <- cutAge(x, labels = TRUE)
  trimmed <- cutAge(x, labels = TRUE, full = FALSE)

  expect_true(is.ordered(full))
  expect_true(is.ordered(trimmed))
  expect_lt(nlevels(trimmed), nlevels(full))
})


test_that("cutAge pads both ends of the label", {

  lv <- levels(cutAge(0:95, labels = TRUE))

  expect_equal(lv[1], "00-09")     # was "00-9"
  expect_equal(lv[2], "10-19")
  expect_equal(lv[length(lv)], "90-..")
})


test_that("cut.integer labels integer ranges and falls back sensibly", {

  x <- as.integer(c(1, 5, 10, 11, 20, 21))

  expect_equal(levels(cut(x, breaks = c(0, 10, 20, Inf))),
               c("1-10", "11-20", "21-.."))

  expect_equal(levels(cut(x, breaks = c(0, 10, 20, Inf), right = FALSE)),
               c("0-9", "10-19", "20-.."))

  # fractional breaks cannot be described as an integer range
  lv <- levels(cut(x, breaks = c(0, 10.5, 21)))
  expect_true(all(grepl("^\\(", lv)))
})


test_that("cutQ handles degenerate and tied input", {

  expect_error(cutQ(rnorm(10), breaks = 1), "at least 2")

  # heavily tied data: the level construction used to index past the end
  # of the bounds table, and reposition() collapsed the 0.8 and 0.9
  # quantiles onto the same observation, which made cut() abort with
  # "'breaks' are not unique"
  x <- c(rep(1, 20), rep(2, 5))
  expect_silent(res <- suppressWarnings(cutQ(x, breaks = 10)))
  expect_false(anyNA(levels(res)))
  expect_length(res, length(x))

  # a single distinct value leaves no interval at all
  expect_silent(suppressWarnings(cutQ(rep(3, 10), breaks = 4)))
})


test_that("divCoef reduces to Gini-Simpson without a distance matrix", {

  x <- cbind(a = c(1, 1, 1, 1), b = c(4, 0, 0, 0), d = c(2, 2, 0, 0))

  expect_equal(unname(divCoef(x)), c(1 - 4 * 0.25^2, 0, 0.5))
})


test_that("divCoef reports missing values instead of aborting", {

  x <- cbind(a = c(1, 1, NA), b = c(1, 1, 1))

  expect_error(divCoef(x), "missing values")
  res <- divCoef(x, na.rm = TRUE)
  expect_true(is.na(res[1]))
  expect_false(is.na(res[2]))
})


test_that("expFreq keeps the table class and reproduces chisq.test", {

  tab <- apply(HairEyeColor, c(1, 2), sum)
  e <- expFreq(as.table(tab))

  expect_s3_class(e, "table")
  expect_equal(unname(as.matrix(e)),
               unname(suppressWarnings(chisq.test(tab)$expected)),
               tolerance = 1e-10)

  expect_equal(sum(expFreq(Titanic, freq = "rel")), 1)
})


test_that("date predicates classify the base classes", {

  d  <- as.Date("2024-01-01")
  dt <- as.POSIXct("2024-01-01 08:00:00", tz = "UTC")

  expect_true(isDate(d));    expect_false(isTime(d));  expect_false(isDateTime(d))
  expect_true(isDate(dt));   expect_true(isTime(dt));  expect_true(isDateTime(dt))
  expect_false(isDate("2024-01-01"))

  same <- as.POSIXct(c("2024-01-01 08:00:00", "2024-01-02 08:00:00"), tz = "UTC")
  expect_false(hasVaryingTime(same))
  expect_true(hasVaryingTime(dt + c(0, 3600)))
  expect_false(hasVaryingTime(d))
})
