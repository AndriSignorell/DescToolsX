

test_that("medianX.Freq works when the first class holds the median", {
  
  # x[mi - 1, "cumfreq"] selected zero rows for mi == 1, so the whole
  # expression collapsed to numeric(0)
  ft <- freq(as.table(c(80, 10, 5, 5)))
  res <- medianX(ft, breaks = c(0, 4000, 6000, 8000, 10000))
  
  expect_length(res, 1L)
  expect_false(is.na(res))
  expect_gte(res, 0); expect_lte(res, 4000)
  
  # and the ordinary case still matches the documented example
  ft2 <- freq(as.table(c(20, 42, 31, 12)))
  expect_length(medianX(ft2, breaks = c(0, 4000, 6000, 8000, 10000)), 1L)
})


test_that("the weighted median is scale invariant", {
  
  x <- c(3.7, 3.3, 3.5, 2.8)
  w <- c(5, 5, 4, 1)
  
  # medianX passed the weights to quantileX with its type-7 default,
  # which reads them as replication counts; normalized weights then
  # collapsed every quantile onto max(x)
  expect_equal(medianX(x, weights = w), medianX(x, weights = w / 15))
  expect_equal(medianX(x, weights = w), medianX(x, weights = w * 7))
  
  # and counts still agree with the replicated sample
  z <- c(2, 5, 9)
  expect_equal(medianX(z, weights = c(3, 1, 2)),
               medianX(rep(z, c(3, 1, 2))))
})


test_that("medianX.factor refuses an unordered factor", {
  
  f <- factor(c("a", "b", "c", "b"))
  expect_error(medianX(f), "unordered")
  
  o <- factor(c("lo", "mid", "hi", "mid"),
              levels = c("lo", "mid", "hi"), ordered = TRUE)
  expect_equal(as.character(medianX(o)), "mid")
})


# Additional coverage: explicit branches and reference results
test_that("medianX delegates numeric and Date inputs to median", {
  expect_equal(medianX(c(1, 4, 9, 12)), 6.5)
  expect_true(is.na(medianX(c(1, NA_real_, 9))))
  expect_equal(medianX(c(1, NA_real_, 9), na.rm = TRUE), 5)
  expect_true(is.na(medianX(numeric())))
  d <- as.Date("2020-01-01") + c(0, 2, 9)
  expect_equal(medianX(d), median(d))
})

test_that("medianX.factor handles missing values and retains all levels", {
  lev <- c("low", "middle", "high", "unused")
  x <- ordered(c("low", NA, "middle", "high"), levels = lev)
  expect_true(is.na(medianX(x)))
  ans <- medianX(x, na.rm = TRUE)
  expect_true(is.ordered(ans))
  expect_identical(levels(ans), lev)
  expect_identical(as.character(ans), "middle")
})

test_that("medianX.factor warns when the middle ranks differ", {
  x <- ordered(c("low", "high"), levels = c("low", "high"))
  expect_warning(ans <- medianX(x), "Median is between two values")
  expect_identical(as.character(ans), "high")
  expect_identical(levels(ans), levels(x))
})

test_that("medianX.Freq checks breaks and interpolates correctly", {
  ft <- freq(as.table(c(80, 10, 5, 5)))
  expect_error(medianX(ft, breaks = c(0, 1)), "one more element")
  expect_equal(medianX(ft, breaks = c(0, 4000, 6000, 8000, 10000)), 2500)
  ft <- freq(as.table(c(20, 42, 31, 12)))
  expect_equal(medianX(ft, breaks = c(0, 4000, 6000, 8000, 10000)),
               4000 + (105 / 2 - 20) / 42 * 2000)
  # Exactly half the mass at a class boundary, with an empty preceding class.
  ft <- freq(as.table(c(0, 5, 5)))
  expect_equal(medianX(ft, breaks = c(0, 10, 20, 40)), 20)
})


test_that("medianX.factor selects an observed category across unused levels", {
  for (lev in list(c("low", "unused", "high"),
                   c("low", "unused1", "unused2", "high"))) {
    x <- ordered(c("low", "high"), levels = lev)
    expect_warning(ans <- medianX(x), "using the upper median")
    expect_identical(as.character(ans), "high")
    expect_identical(levels(ans), lev)
  }
})

test_that("medianX.factor returns a typed missing value for empty or missing data", {
  lev <- c("low", "middle", "high")
  expected <- ordered(NA_character_, levels = lev)
  for (x in list(ordered(character(), levels = lev),
                 ordered(c(NA_character_, NA_character_), levels = lev))) {
    expect_identical(medianX(x), expected)
    expect_identical(medianX(x, na.rm = TRUE), expected)
  }
  expect_identical(medianX(ordered(character())), ordered(NA_character_, levels = character()))
  x <- ordered(c("low", NA, "high"), levels = lev)
  expect_identical(medianX(x), expected)
  expect_warning(ans <- medianX(x, na.rm = TRUE), "using the upper median")
  expect_identical(as.character(ans), "high")
})

test_that("medianX.factor warns only for distinct middle observations", {
  lev <- c("low", "middle", "high")
  for (values in list("middle", c("high", "middle", "low"),
                      c("low", "middle", "middle", "high"))) {
    expect_silent(ans <- medianX(ordered(values, levels = lev)))
    expect_identical(as.character(ans), "middle")
  }
})


# Review 25.09.2026: median class via cumfreq, open classes, breaks, weights
test_that("a class ending at exactly half the mass gives its upper bound", {
  # the following class is empty; cumperc > 0.5 skipped it and returned the
  # lower bound of the next non-empty class (30) instead
  ft <- freq(as.table(c(5, 5, 0, 10)))
  expect_equal(medianX(ft, breaks = c(0, 10, 20, 30, 40)), 20)
})

test_that("open-ended classes: NA with warning only when they hold the median", {
  br <- c(-Inf, 10, 20, Inf)
  expect_warning(m <- medianX(freq(as.table(c(60, 10, 5))), breaks = br),
                 "open-ended")
  expect_identical(m, NA_real_)
  expect_no_warning(m <- medianX(freq(as.table(c(5, 60, 5))), breaks = br))
  expect_equal(m, 10 + (35 - 5) / 60 * 10)
})

test_that("medianX.Freq rejects breaks that are not strictly increasing", {
  ft <- freq(as.table(c(20, 42, 31)))
  expect_error(medianX(ft, breaks = c(0, 20, 10, 30)), "strictly increasing")
  expect_error(medianX(ft, breaks = c(0, 10, 10, 30)), "strictly increasing")
})

test_that("frequency weights reproduce the median of the replicated sample", {
  set.seed(7)
  for (n in c(30, 31)) {
    x <- sample(20, n, replace = TRUE)
    w <- table(x)
    z <- as.numeric(names(w))
    expect_equal(medianX(z, weights = as.numeric(w)), median(x), info = n)
    expect_equal(medianX(z, weights = as.numeric(w) / n), median(x), info = n)
  }
})

test_that("weights are rejected for ordered factors instead of ignored", {
  o <- ordered(c("lo", "mid", "hi", "mid"), levels = c("lo", "mid", "hi"))
  expect_error(medianX(o, weights = c(1, 1, 5, 1)), "not supported")
})
