

test_that("desc.formula handles a numeric RHS term (numeric-numeric)", {
  
  set.seed(1)
  df <- data.frame(y = rnorm(30), x = rnorm(30), g = rep(c("A","B"), 15))
  
  # regression guard: resolveFormula() uses 'predictor', not 'group',
  # for numeric-numeric designs - desc.formula() must read rf$predictor
  # here, or this throws "Unknown type combination: n".
  expect_no_error(res <- desc(y ~ x, data = df))
  expect_s3_class(res[["x"]], "Desc.nn")
  
  # n-sample-independent (categorical RHS) remains unaffected
  expect_no_error(desc(y ~ g, data = df))
})

