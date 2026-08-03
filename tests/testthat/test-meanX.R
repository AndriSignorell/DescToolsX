

test_that("meanX passes trim and na.rm by name", {
  
  x <- c(0:10, 50)
  expect_equal(meanX(x), mean(x))
  expect_equal(meanX(x, trim = 0.1), mean(x, trim = 0.1))
  
  # a Date method takes everything but x through its dots
  d <- as.Date("2020-01-01") + c(0, 10, 20)
  expect_equal(meanX(d), mean(d))
  expect_equal(meanX(c(d, NA), na.rm = TRUE), mean(c(d, NA), na.rm = TRUE))
})


