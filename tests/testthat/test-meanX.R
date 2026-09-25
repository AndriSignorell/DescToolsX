

test_that("meanX passes trim and na.rm by name", {
  
  x <- c(0:10, 50)
  expect_equal(meanX(x), mean(x))
  expect_equal(meanX(x, trim = 0.1), mean(x, trim = 0.1))
  
  # a Date method takes everything but x through its dots
  d <- as.Date("2020-01-01") + c(0, 10, 20)
  expect_equal(meanX(d), mean(d))
  expect_equal(meanX(c(d, NA), na.rm = TRUE), mean(c(d, NA), na.rm = TRUE))
})


# Additional coverage: explicit branches and reference results
test_that("meanX weighted mean agrees with replication and weighted.mean", {
  x <- c(2, 7, 13)
  w <- c(3, 1, 2)
  expect_equal(meanX(x, weights = w), mean(rep(x, w)))
  expect_equal(meanX(x, weights = w / sum(w)), weighted.mean(x, w))
  expect_equal(meanX(c(x, 999), weights = c(w, 0)), weighted.mean(x, w))
  expect_equal(meanX(c(x, NA_real_), weights = c(w, 4), na.rm = TRUE),
               weighted.mean(x, w))
})

test_that("meanX warns and ignores trim when weights are supplied", {
  x <- c(1, 2, 3, 100)
  w <- c(1, 2, 3, 1)
  expect_warning(ans <- meanX(x, weights = w, trim = 0.25),
                 "trim can't be set together with weights")
  expect_equal(ans, weighted.mean(x, w))
})

test_that("meanX.Freq uses class midpoints including unequal widths", {
  ft <- freq(as.table(c(2, 3, 5)))
  expect_equal(meanX(ft, breaks = c(0, 2, 6, 12)),
               (2 * 1 + 3 * 4 + 5 * 9) / 10)
})

test_that("meanX preserves time classes and delegates missing values", {
  x <- as.POSIXct("2020-01-01", tz = "UTC") + c(0, 60, 120)
  # the NA must carry the same tzone: c(x, NA) combines with a logical NA,
  # and c.POSIXct() drops tzone when the parts disagree - that was the
  # test's doing, not meanX's
  xNA <- c(x, x[NA_integer_])
  expect_identical(attr(xNA, "tzone"), "UTC")
  expect_equal(meanX(xNA, na.rm = TRUE), mean(x))
  d <- as.difftime(c(1, 3, 8), units = "hours")
  expect_equal(meanX(d), mean(d))
  expect_true(is.na(meanX(c(1, NA_real_))))
  expect_equal(meanX(c(1, NA_real_), na.rm = TRUE), 1)
})
