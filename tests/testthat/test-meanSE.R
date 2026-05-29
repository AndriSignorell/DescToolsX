test_that("meanSE returns a positive numeric for positive data", {
  x <- c(1, 2, 3, 4, 5)
  res <- meanSE(x)
  expect_gte(res, 0)
  expect_length(res, 1)
})

test_that("meanSE equals sd(x)/sqrt(n) by default", {
  x <- c(3, 7, 2, 9, 5)
  expect_equal(meanSE(x), sd(x)/sqrt(length(x)), tolerance = 1e-10)
})

test_that("meanSE with custom sd uses that sd instead of sample sd", {
  x <- c(1, 2, 3, 4, 5)
  pop_sd <- 2
  expect_equal(meanSE(x, sd = pop_sd), pop_sd / sqrt(length(x)))
})

test_that("meanSE decreases as n increases", {
  se5  <- meanSE(rnorm(5,   mean=5, sd=2))
  se50 <- meanSE(rnorm(50,  mean=5, sd=2))
  # not guaranteed for random data, use fixed data instead
  x5  <- rep(c(3,7), length.out=5)
  x50 <- rep(c(3,7), length.out=50)
  expect_gt(meanSE(x5), meanSE(x50))
})

test_that("meanSE na.rm = TRUE strips NAs", {
  x <- c(1, 2, NA, 4, 5)
  expect_equal(meanSE(x, na.rm = TRUE), meanSE(c(1,2,4,5)))
})

test_that("meanSE returns NA for a single value (sd = NA)", {
  expect_true(is.na(meanSE(5)))
})
