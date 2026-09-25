

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


# Review 25.09.2026: containers, header, data frames ---------------------------

withr::local_options(list(DescToolsX.plotit = FALSE))

test_that("desc.list describes every element, named or not", {
  res <- desc(list(a = c(1.5, 2.5, 3.5, 7), c("x", "y", "y", "z")))
  expect_s3_class(res, c("Desc.list", "Desc"))
  expect_identical(names(res$data), c("a", "[[2]]"))
  expect_s3_class(res$data$a, "Desc.numeric")
  expect_s3_class(res$data[["[[2]]"]], "Desc.factor")
  expect_identical(res$data$a$meta$main, "a")
  # an unnamed list used to give an empty result
  expect_length(desc(list(1:5 + 0.5, 6:10 + 0.5))$data, 2L)
})

test_that("desc.data.frame adds an abstract and prints every column", {
  d.x <- data.frame(num = c(2.1, 3.4, 1.8, 5.5, 4.2),
                    grp = factor(c("a", "b", "a", "c", "b")))
  res <- desc(d.x)
  expect_identical(names(res$data), c("num", "grp"))
  expect_match(attr(res$abstract, "main"), "Describe d.x (data.frame)",
               fixed = TRUE)
  out <- capture.output(r <- print(res))
  expect_identical(r, res)
  expect_true(any(grepl("num (numeric)", out, fixed = TRUE)))
  expect_true(any(grepl("grp (factor)", out, fixed = TRUE)))
})

test_that("a data frame with several classes gets one header, not several", {
  d.x <- data.frame(num = c(2.1, 3.4, 1.8, 5.5))
  class(d.x) <- c("myframe", "data.frame")
  res <- desc(d.x)
  expect_length(attr(res$abstract, "main"), 1L)
  expect_match(attr(res$abstract, "main"), "myframe, data.frame", fixed = TRUE)
})

test_that("the header shows main and label, and main = NA suppresses it", {
  x <- c(1.2, 3.4, 2.2, 5.1, 4.4)
  attr(x, "label") <- "Body height in m"
  expect_output(print(desc(x)), "Body height in m")
  out <- capture.output(print(desc(x, main = "height")))
  expect_true(any(grepl("height (numeric)", out, fixed = TRUE)))
  out <- capture.output(print(desc(c(1.2, 3.4, 2.2), main = NA)))
  expect_false(any(grepl("(numeric)", out, fixed = TRUE)))
})

test_that("plot.Desc and print.Desc walk a formula result", {
  local_null_device()
  set.seed(4)
  df <- data.frame(y = rnorm(20), x = rnorm(20))
  res <- desc(y ~ x, data = df)
  expect_output(r <- print(res), "Pearson")
  expect_identical(r, res)
  expect_no_error(plot(res))
})
