test_that("freq returns an object of class 'Freq'", {
  x <- factor(c("A","B","A","C","B","A"))
  expect_s3_class(freq(x), "Freq")
})

test_that("freq has exactly 5 columns", {
  x <- factor(c("A","B","A","C"))
  expect_equal(ncol(freq(x)), 5L)
})

test_that("freq column names are level, freq, perc, cumfreq, cumperc", {
  x <- factor(c("A","B","A"))
  expect_named(freq(x), c("level","freq","perc","cumfreq","cumperc"))
})

test_that("freq absolute counts sum to length of x (no NA, no useNA)", {
  x <- factor(c("A","B","A","C","B","A"))
  expect_equal(sum(freq(x)$freq), length(x))
})

test_that("freq relative frequencies sum to 1", {
  x <- factor(c("A","B","A","C"))
  expect_equal(sum(freq(x)$perc), 1, tolerance = 1e-10)
})

test_that("freq cumulative frequency ends at n", {
  x <- factor(c("A","B","A","C"))
  ft <- freq(x)
  expect_equal(tail(ft$cumfreq, 1), length(x))
})

test_that("freq cumulative percentage ends at 1", {
  x <- factor(c("A","B","A","C"))
  ft <- freq(x)
  expect_equal(tail(ft$cumperc, 1), 1, tolerance = 1e-10)
})

test_that("freq ord = 'desc' sorts by descending frequency", {
  x <- factor(c("A","A","A","B","B","C"))
  ft <- freq(x, ord = "desc")
  expect_equal(ft$level[1], "A")
})

test_that("freq ord = 'asc' sorts by ascending frequency", {
  x <- factor(c("A","A","A","B","B","C"))
  ft <- freq(x, ord = "asc")
  expect_equal(ft$level[1], "C")
})

test_that("freq useNA = 'ifany' includes NA as a level when present", {
  x <- factor(c("A","B",NA))
  ft <- freq(x, useNA = "ifany")
  expect_true("<NA>" %in% ft$level)
})

test_that("freq useNA = 'no' (default) excludes NA", {
  x <- factor(c("A","B",NA))
  ft <- freq(x, useNA = "no")
  expect_false("<NA>" %in% ft$level)
})

test_that("freq accepts a pre-built table object", {
  tab <- as.table(c(A = 3, B = 2, C = 1))
  ft  <- freq(tab)
  expect_s3_class(ft, "Freq")
  expect_equal(sum(ft$freq), 6L)
})

test_that("freq medianclass attribute is set", {
  x <- factor(c("A","A","B","B","C","C"))
  ft <- freq(x)
  expect_false(is.null(attr(ft, "medianclass")))
})

test_that("freq print method works without error", {
  x <- factor(c("A","B","A","C"))
  expect_output(print(freq(x)))
})


test_that("freq actually sorts by level name", {
  
  x <- factor(c("b", "b", "a", "c", "c", "c"),
              levels = c("c", "b", "a"))   # levels NOT alphabetical
  
  byLevel <- freq(x, ord = "level")
  byName  <- freq(x, ord = "name")
  
  expect_equal(as.character(byLevel$level), c("c", "b", "a"))
  expect_equal(as.character(byName$level),  c("a", "b", "c"))
})


# Review 25.09.2026 ------------------------------------------------------------

test_that("a numeric vector without finite values gives an empty table", {
  # the default breaks called hist(), which fails without a finite value
  expect_no_error(ft <- freq(c(NA_real_, NA_real_)))
  expect_equal(nrow(ft), 0L)
  ft <- freq(c(NA_real_, NA_real_), useNA = "ifany")
  expect_equal(ft$freq, 2)
  expect_identical(ft$level, "<NA>")
})

test_that("numeric and date vectors are binned, breaks = FALSE keeps values", {
  x <- c(1, 2, 2, 3, 7, 8, 9, 9, 10)
  ft <- freq(x, breaks = c(0, 5, 10))
  expect_equal(ft$freq, c(4, 5))
  expect_equal(freq(x, breaks = FALSE)$freq, as.vector(table(x)))
  d <- as.Date("2024-01-01") + c(0, 10, 40, 45, 70)
  expect_equal(sum(freq(d, breaks = "month")$freq), 5)
})

test_that("the median class is the first reaching half the mass", {
  # cumulative shares 1/6, 2/6, 1: "c" is the first to reach 0.5
  ft <- freq(factor(c("a", "b", "c", "c", "c", "c")))
  expect_identical(attr(ft, "medianclass"), "c")
})

test_that("print.Freq honours digits and falls back for altered objects", {
  ft <- freq(factor(c("a", "b", "b")))
  expect_output(expect_invisible(print(ft, digits = 3)), "66.667")
  ft$extra <- 1
  expect_output(print(ft), "extra")
})
