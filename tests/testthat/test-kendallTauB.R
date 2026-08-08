


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

