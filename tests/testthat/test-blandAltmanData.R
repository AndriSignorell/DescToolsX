set.seed(42)
x <- rnorm(30, 100, 10)
y <- x + rnorm(30, 2, 3)

test_that("blandAltmanData() computes bias, LoA and their intervals", {
  ba <- blandAltmanData(x, y)
  d  <- y - x
  s  <- sd(d)
  tq <- qt(0.975, df = 29)

  expect_s3_class(ba, "BlandAltman")
  expect_equal(ba$mean, (x + y) / 2)
  expect_equal(ba$diff, d)
  expect_equal(ba$bias, mean(d))
  expect_equal(ba$loaLower, mean(d) - qnorm(0.975) * s)
  expect_equal(ba$loaUpper, mean(d) + qnorm(0.975) * s)
  expect_equal(ba$biasCI, mean(d) + c(-1, 1) * tq * s / sqrt(30))
  expect_equal(ba$loaLowerCI, ba$loaLower + c(-1, 1) * tq * sqrt(3 * s^2 / 30))
  expect_equal(ba$loaUpperCI, ba$loaUpper + c(-1, 1) * tq * sqrt(3 * s^2 / 30))
  expect_identical(ba$nObs, 30L)
})

test_that("conf.level changes the intervals but not the limits", {
  ba95 <- blandAltmanData(x, y)
  ba90 <- blandAltmanData(x, y, conf.level = 0.90)
  expect_equal(ba90$loaLower, ba95$loaLower)
  expect_equal(ba90$loaUpper, ba95$loaUpper)
  expect_lt(diff(ba90$biasCI), diff(ba95$biasCI))
  expect_identical(ba90$conf.level, 0.90)
})

test_that("na.rm removes incomplete pairs, otherwise NAs are an error", {
  xx <- replace(x, 3, NA)
  expect_error(blandAltmanData(xx, y), "na.rm = TRUE")
  ba <- blandAltmanData(xx, y, na.rm = TRUE)
  expect_identical(ba$nObs, 29L)
  expect_equal(ba$bias, mean(y[-3] - x[-3]))
})

test_that("blandAltmanData() validates its input", {
  expect_error(blandAltmanData(letters[1:5], 1:5), "'x' must be numeric")
  expect_error(blandAltmanData(1:5, letters[1:5]), "'y' must be numeric")
  expect_error(blandAltmanData(1:5, 1:4), "equal lengths")
  for (cl in list(0, 1, NA_real_, c(0.9, 0.95), "0.95"))
    expect_error(blandAltmanData(x, y, conf.level = cl), "conf.level")
  expect_error(blandAltmanData(x, y, na.rm = NA), "na.rm")
  expect_error(blandAltmanData(c(1, 2), c(1, 3)), "At least 3")
  expect_error(blandAltmanData(c(1, 2, Inf), c(1, 2, 3)), "finite")
})

test_that("print.BlandAltman() formats with 'digits' decimals", {
  ba <- blandAltmanData(x, y)
  expect_output(expect_invisible(print(ba)), "Bland-Altman")
  expect_output(print(ba, digits = 2), sprintf("%.2f", ba$bias), fixed = TRUE)
  expect_output(print(ba), "n = 30")
  expect_error(print(ba, digits = -1), "digits")
  expect_error(print(ba, digits = 1.5), "digits")
})

test_that("the formula method agrees with the default method", {
  # lhs ~ rhs gives diff = lhs - rhs: rhs is the reference (x), lhs is y
  d <- data.frame(a = x, b = y)
  expect_equal(blandAltmanData(b ~ a, data = d),
               blandAltmanData(d$a, d$b))
  expect_equal(blandAltmanData(b ~ a, data = d)$diff, d$b - d$a)
  expect_equal(blandAltmanData(b ~ a, data = d, conf.level = 0.9),
               blandAltmanData(d$a, d$b, conf.level = 0.9))
})

test_that("the formula method rejects a categorical right-hand side", {
  d <- data.frame(a = factor(rep(c("u", "v"), 15)), b = y)
  expect_error(blandAltmanData(b ~ a, data = d))
})
