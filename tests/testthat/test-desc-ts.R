withr::local_options(list(DescToolsX.plotit = FALSE))

test_that("capped ADF/KPSS p-values become a bound, not a warning", {
  expect_no_warning(d <- desc(Nile))
  expect_true(attr(d$kpss, "pBound") %in% c("<", ">", NA))
  expect_no_warning(desc(AirPassengers))
  capped <- Filter(function(b) !is.na(b),
                   list(attr(d$adf, "pBound"), attr(d$kpss, "pBound")))
  if (length(capped))
    expect_output(print(d), paste0("p = ", capped[[1]]), fixed = TRUE)
})

test_that("desc.ts() reports counts and time attributes", {
  d <- desc(Nile)
  expect_s3_class(d, c("Desc.ts", "Desc"))
  expect_identical(d$length, 100L)
  expect_identical(d$n, 100L)
  expect_identical(d$NAs, 0L)
  expect_equal(d$frequency, 1)
  expect_equal(d$start, c(1871, 1))
  expect_equal(d$end, c(1970, 1))
  expect_equal(desc(AirPassengers)$frequency, 12)
})

test_that("desc.ts() diagnostics agree with the reference functions", {
  d <- desc(Nile)
  expect_equal(d$acf1, acf(Nile, plot = FALSE)$acf[2])
  expect_equal(d$ljungbox$statistic,
               Box.test(Nile, lag = 12, type = "Ljung")$statistic)
  tt <- as.numeric(time(Nile))
  expect_equal(unname(d$trend["slope"]),
               unname(coef(lm(as.numeric(Nile) ~ tt))[2]))
  expect_equal(d$boxcoxlambda, boxCoxLambda(as.numeric(Nile)))
  expect_identical(d$stationary,
                   d$adf$p.value < 0.05 && d$kpss$p.value > 0.05)
})

test_that("maxLag is passed to the Ljung-Box test", {
  expect_equal(unname(desc(Nile, maxLag = 10)$ljungbox$parameter), 10)
})

test_that("desc.ts() tolerates missing values", {
  y <- Nile
  y[c(10, 50)] <- NA
  d <- desc(y)
  expect_identical(d$n, 98L)
  expect_identical(d$NAs, 2L)
  expect_true(is.finite(d$acf1))
  expect_true(is.finite(d$ljungbox$statistic))
})

test_that("Box-Cox lambda is NA for non-positive series", {
  z <- ts(sin(1:60) + (1:60) / 20 - 1)
  expect_true(is.na(desc(z)$boxcoxlambda))
})

test_that("multivariate series are rejected", {
  expect_error(desc(ts(matrix(rnorm(20), 10))), "univariate")
})

test_that("print.Desc.ts shows the diagnostics and returns invisibly", {
  d <- desc(Nile)
  expect_output(expect_invisible(print(d)), "Ljung-Box")
  expect_output(print(d), "stationary")
  expect_output(print(desc(ts(sin(1:60) + (1:60) / 20 - 1))), "Box-Cox lambda")
})

test_that("plot.Desc.ts draws without error", {
  local_null_device()
  expect_no_error(plot(desc(Nile)))
})
