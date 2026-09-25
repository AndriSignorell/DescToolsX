withr::local_options(list(DescToolsX.plotit = FALSE))

x <- factor(c("a", "b", "b", "c", "c", "c", NA), levels = c("a", "b", "c", "d"))

test_that("desc.factor() counts levels, values and missings", {
  d <- desc(x)
  expect_s3_class(d, c("Desc.factor", "Desc"))
  expect_identical(d$length, 7L)
  expect_identical(d$n, 6L)
  expect_identical(d$NAs, 1L)
  expect_identical(d$levels, 4L)
  expect_identical(d$unique, 3L)
  expect_true(d$dupes)
  expect_identical(d$maxrows, 12)
})

test_that("unordered factors sort by descending frequency, ordered by level", {
  expect_identical(desc(x)$ord, "desc")
  expect_identical(as.character(desc(x)$freq[[1]][1]), "c")
  xo <- factor(x, ordered = TRUE)
  expect_identical(desc(xo)$ord, "level")
  expect_identical(as.character(desc(xo)$freq[[1]][1]), "a")
  expect_identical(
    as.character(desc(x, ord = "name")$freq[[1]][1]), "a")
})

test_that("character vectors are described like factors", {
  d <- desc(c("u", "v", "v", "w", "w", "w"))
  expect_s3_class(d, "Desc.factor")
  expect_identical(d$levels, 3L)   # nlevels() of a character is 0
  expect_identical(d$unique, 3L)
})

test_that("maxrows < 1 is read as a cumulative proportion", {
  f <- factor(rep(c("a", "b", "c", "d"), times = c(50, 30, 15, 5)))
  expect_equal(desc(f, maxrows = 0.7)$maxrows, 2)
  expect_equal(desc(f, maxrows = 3)$maxrows, 3)
})

test_that("print.Desc.factor truncates long tables and returns invisibly", {
  f <- factor(sprintf("L%02d", rep(1:15, 15:1)))
  expect_output(expect_invisible(print(desc(f))), "truncated")
  out <- capture.output(print(desc(x)))
  expect_false(any(grepl("truncated", out)))
  expect_output(print(desc(x), digits = 3))
})

test_that("plot.Desc.factor draws without error", {
  local_null_device()
  expect_no_error(plot(desc(x)))
  expect_output(print(desc(x, plotit = TRUE)))
})
