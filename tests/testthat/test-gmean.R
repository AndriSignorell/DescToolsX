test_that("gmean returns the geometric mean for positive values", {
  x <- c(1, 2, 4, 8)
  expect_equal(gmean(x), exp(mean(log(x))), tolerance = 1e-10)
})

test_that("gmean is <= arithmetic mean (AM-GM inequality)", {
  x <- c(2, 8, 18, 32)
  expect_lte(gmean(x), mean(x))
})

test_that("gmean equals arithmetic mean for a constant vector", {
  x <- rep(5, 10)
  expect_equal(gmean(x), 5)
})

test_that("gmean returns NA when x contains negative values", {
  expect_warning(res <- gmean(c(1, -2, 3)), "negative values")
  expect_true(is.na(res))
})

test_that("gmean returns 0 when x contains a zero", {
  expect_equal(gmean(c(1, 0, 4)), 0)
})

test_that("gmean na.rm = TRUE strips NAs", {
  x <- c(1, 2, NA, 4)
  expect_equal(gmean(x, na.rm = TRUE), gmean(c(1, 2, 4)))
})

test_that("gmean returns NA (not error) when NA present and na.rm = FALSE", {
  expect_true(is.na(gmean(c(1, 2, NA))))
})

test_that("gmean conf.level returns a 3-element vector", {
  set.seed(1)
  x <- exp(rnorm(50))
  res <- gmean(x, conf.level = 0.95)
  expect_length(res, 3)
})

test_that("gmean CI: lci < estimate < uci", {
  set.seed(2)
  x <- exp(rnorm(80))
  res <- gmean(x, conf.level = 0.95)
  expect_lt(res[2], res[1])
  expect_gt(res[3], res[1])
})



test_that("gsd survives a zero when na.rm = TRUE", {
  
  expect_equal(gsd(c(1, 2, 4), na.rm = TRUE), gsd(c(1, 2, 4)))
  expect_false(is.na(gsd(c(1, 2, 0, 4), na.rm = TRUE)))
  expect_equal(gsd(c(1, 2, 0, 4), na.rm = TRUE), gsd(c(1, 2, 4)))
  
  expect_true(is.na(gsd(c(1, 2, 0, 4))))   # na.rm = FALSE
})


# Additional branch coverage and reference checks
test_that("gmean preserves interval shape for negative, missing and zero data", {
  expect_warning(ans <- gmean(c(1, -2, NA), conf.level = 0.95), "negative values")
  expect_identical(ans, c(est = NA_real_, lci = NA_real_, uci = NA_real_))
  expect_silent(ans <- gmean(c(1, NA_real_), conf.level = 0.95))
  expect_true(all(is.na(ans)))
  expect_named(ans, c("est", "lci", "uci"))
  expect_equal(gmean(c(1, 0, 4), conf.level = 0.95), c(est = 0, lci = NA_real_, uci = NA_real_))
})

test_that("gmean classic limits agree with a t interval on log data", {
  z <- c(-1, 0, 1, 2)
  x <- exp(z)
  half <- qt(0.975, 3) * sd(z) / 2
  expect_equal(unname(gmean(x, conf.level = 0.95, method = "classic")),
               exp(c(mean(z), mean(z) - half, mean(z) + half)))
})

test_that("gsd removes nonpositive values only when requested", {
  expect_equal(gsd(c(1, exp(1), exp(2))), exp(1))
  expect_equal(gsd(c(0, -1, NA, 1, exp(1), exp(2)), na.rm = TRUE), exp(1))
  expect_true(is.na(gsd(c(1, -1))))
  expect_identical(gsd(numeric()), NA_real_)
  expect_identical(gsd(1), NA_real_)
  expect_identical(gsd(c(0, -1, NA), na.rm = TRUE), NA_real_)
})
