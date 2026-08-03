

test_that("kendallTauA and kendallTauB forward their dots", {
  
  x <- c(1, 2, 2, 3, 3, 3, 4, 5, NA)
  y <- c(1, 3, 2, 1, 5, 3, 4, 5, 2)
  
  # ... was documented as reaching table() and never forwarded at all
  expect_silent(kendallTauB(x, y, direction = "row"))
  expect_silent(kendallTauA(x, y, direction = "row"))
})


test_that("kendallTauB agrees with cor(method = 'kendall')", {
  
  set.seed(5)
  x <- round(rnorm(60), 1)
  y <- round(x + rnorm(60), 1)
  
  expect_equal(kendallTauB(x, y), cor(x, y, method = "kendall"),
               tolerance = 1e-8)
  
  # documented symmetry
  expect_equal(kendallTauA(x, y), kendallTauA(y, x))
  expect_equal(kendallTauB(x, y), kendallTauB(y, x))
  
  # and an unnamed scalar without conf.level
  expect_null(names(kendallTauA(x, y)))
  expect_null(names(kendallTauB(x, y)))
})

