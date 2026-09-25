test_that(".notThere warns with the class and returns NA", {
  obj <- structure(list(), class = c("foo", "bar"))
  expect_warning(res <- .notThere(obj), "foo, bar")
  expect_identical(res, NA_real_)
})

test_that(".hasColor follows cli's colour detection", {
  withr::local_options(list(cli.num_colors = 1L))
  expect_false(.hasColor())
  withr::local_options(list(cli.num_colors = 256L))
  expect_true(.hasColor())
})

test_that(".captOut evaluates in the caller's frame, with a wide console", {
  x <- list(v = 1:60)
  w <- getOption("width")
  out <- .captOut(x$v)
  expect_identical(getOption("width"), w)         # restored
  # width 150 needs fewer lines than the test console
  expect_lt(length(out), length(capture.output(print(x$v))))
  expect_identical(.captOut(1, "a"), c("[1] 1", "[1] \"a\""))

  tmp <- tempfile()
  on.exit(unlink(tmp))
  .captOut(1:3, file = tmp)
  .captOut("b", file = tmp, append = TRUE)
  expect_identical(readLines(tmp), c("[1] 1 2 3", "[1] \"b\""))
})

test_that(".makeEstimateResult strips names and sets attributes", {
  res <- .makeEstimateResult(2, quantile(1:10, 0.025), quantile(1:10, 0.975),
                             attrs = list(method = "perc", n = 10))
  expect_named(res, c("est", "lci", "uci"))
  expect_identical(attr(res, "method"), "perc")
  expect_identical(attr(res, "n"), 10)
  expect_identical(.makeEstimateResult(c(a = 1)), c(est = 1))
})

test_that(".chisqNcpCI inverts the noncentral chi-squared distribution", {
  ci <- .chisqNcpCI(20, df = 4, conf = 0.95)
  expect_equal(pchisq(20, 4, ncp = ci[["lower"]]), 0.975, tolerance = 1e-5)
  expect_equal(pchisq(20, 4, ncp = ci[["upper"]]), 0.025, tolerance = 1e-5)
  # a small statistic: even lambda = 0 is below the upper target
  expect_identical(.chisqNcpCI(1, df = 4)[["lower"]], 0)
  expect_gt(.chisqNcpCI(1, df = 4)[["upper"]], 0)
  # a tiny one: both bounds are 0
  expect_identical(unname(.chisqNcpCI(1e-4, df = 4)), c(0, 0))
  expect_identical(.chisqNcpCI(0, df = 4), c(lower = 0, upper = NA))
  expect_error(.chisqNcpCI(-1, df = 4), ">= 0")
})

test_that(".chisqNcpCI handles statistics beyond pchisq's ncp range", {
  # the lower bound used to be 0 here (bracket too short), and pchisq()
  # with ncp in the millions does not converge
  expect_no_warning(ci <- .chisqNcpCI(2e6, df = 3))
  # normal approximation: lambda +/- 1.96 * sqrt(2 * (df + 2 lambda))
  expect_gt(ci[["lower"]], 1.99e6)
  expect_lt(ci[["lower"]], 2e6)
  expect_gt(ci[["upper"]], 2e6)
  expect_lt(ci[["upper"]], 2.01e6)
})

test_that(".onLoad has set the package options", {
  expect_length(getOption("DescToolsX.footnote"), 9L)
  expect_identical(getOption("DescToolsX.lang"), "en")
})
