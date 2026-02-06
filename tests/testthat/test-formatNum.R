
test_that("formatNum works", {
  x <- c(1.2345, 1000.2)
  expect_type(formatNum(x), "character")
})

