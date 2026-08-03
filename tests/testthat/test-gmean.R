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
  expect_true(is.na(gmean(c(1, -2, 3))))
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


