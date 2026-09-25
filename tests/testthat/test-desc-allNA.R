withr::local_options(list(DescToolsX.plotit = FALSE))

test_that("all-NA and zero-length input yields Desc.AllNA for every method", {
  cases <- list(
    numeric   = c(NA_real_, NA_real_),
    factor    = factor(c(NA, NA), levels = "a"),
    character = NA_character_,
    logical   = c(NA, NA, NA),
    Date      = as.Date(c(NA, NA)),
    ts        = ts(c(NA_real_, NA_real_)),
    empty     = numeric(0)
  )
  for (nm in names(cases)) {
    d <- desc(cases[[nm]])
    expect_s3_class(d, "Desc.AllNA")
    expect_identical(d$n, 0L, info = nm)
    expect_identical(d$length, length(cases[[nm]]), info = nm)
    expect_identical(d$NAs, length(cases[[nm]]), info = nm)
  }
})

test_that("print.Desc.AllNA shows the counts and returns invisibly", {
  d <- desc(c(NA_real_, NA_real_, NA_real_))
  expect_output(expect_invisible(print(d)), "NAs")
})

test_that("plot.Desc.AllNA and print with plotit = TRUE draw without error", {
  local_null_device()
  d <- desc(c(NA_real_, NA_real_), plotit = TRUE)
  expect_no_error(plot(d))
  expect_output(print(d))
})
