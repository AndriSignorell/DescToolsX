withr::local_options(list(DescToolsX.plotit = FALSE))

# Wilson score interval, independent reference
wilson <- function(k, n, conf.level = 0.95) {
  z   <- qnorm(1 - (1 - conf.level) / 2)
  p   <- k / n
  ctr <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  hw  <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(p, ctr - hw, ctr + hw)
}

x <- c(TRUE, FALSE, TRUE, TRUE, NA)

test_that("desc.logical() counts and uses Wilson intervals", {
  d <- desc(x)
  expect_s3_class(d, c("Desc.logical", "Desc"))
  expect_identical(d$length, 5L)
  expect_identical(d$n, 4L)
  expect_identical(d$NAs, 1L)
  expect_identical(d$unique, 2L)
  expect_identical(names(d$afrq), c("FALSE", "TRUE"))
  expect_equal(as.vector(d$afrq), c(1, 3))
  expect_equal(unname(d$rfrq[2, ]), wilson(3, 4))
  expect_equal(unname(d$rfrq[1, ]), wilson(1, 4))
})

test_that("conf.level is passed to the intervals", {
  d <- desc(x, conf.level = 0.90)
  expect_equal(unname(d$rfrq[2, ]), wilson(3, 4, 0.90))
  expect_identical(d$conf.level, 0.90)
})

test_that("a constant logical gives a single row", {
  d <- desc(c(TRUE, TRUE, TRUE))
  expect_identical(d$unique, 1L)
  expect_identical(nrow(d$rfrq), 1L)
  expect_equal(unname(d$rfrq[1, ]), wilson(3, 3))
})

test_that("0/1 numerics and two-valued factors/characters are routed here", {
  expect_s3_class(desc(c(0, 1, 1, 0, 1)), "Desc.logical")
  expect_s3_class(desc(factor(c("no", "yes", "yes"))), "Desc.logical")
  expect_s3_class(desc(c("a", "b", "a")), "Desc.logical")
  # only 0/1 qualifies for numerics
  expect_s3_class(desc(c(3.2, 7.8, 3.2)), "Desc.numeric")
})

test_that("'ord' orders the frequency table", {
  f <- factor(c("b", "b", "b", "a"), levels = c("b", "a"))
  expect_identical(names(desc(f, ord = "level")$afrq), c("b", "a"))
  expect_identical(names(desc(f, ord = "name")$afrq),  c("a", "b"))
  expect_identical(names(desc(f, ord = "asc")$afrq),   c("a", "b"))
  expect_identical(names(desc(f, ord = "desc")$afrq),  c("b", "a"))
  expect_error(desc(f, ord = "foo"))
})

test_that("include_x = FALSE drops the data", {
  expect_null(desc(x, include_x = FALSE)$x)
  expect_identical(desc(x)$x, x)
})

test_that("print.Desc.logical reports the interval and returns invisibly", {
  d <- desc(x)
  expect_output(expect_invisible(print(d)), "Wilson")
  expect_output(print(d, digits = 3), "lci")
  expect_output(print(desc(c(TRUE, TRUE))), "Wilson")
})

test_that("plot.Desc.logical draws, also for a constant vector", {
  local_null_device()
  expect_no_error(plot(desc(x)))
  expect_no_error(plot(desc(c(TRUE, TRUE, TRUE))))
  expect_output(print(desc(x, plotit = TRUE)))
})
