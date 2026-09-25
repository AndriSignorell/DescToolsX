withr::local_options(list(DescToolsX.plotit = FALSE))

d <- data.frame(
  a = factor(c("x", "y", "x", "y", "x", "x", "y", "y", "x", "y", "x", "y")),
  b = factor(c("u", "u", "v", "v", "u", "v", "u", "v", "u", "v", "w", "w"))
)

test_that(".descQQ() is desc() of the cross table", {
  r <- .descQQ(d$a, d$b)
  expect_s3_class(r, "Desc.table")
  expect_equal(as.vector(r$tab), as.vector(table(d$a, d$b)))
  expect_identical(r$ttype, "trxc")
})

test_that("desc(factor ~ factor) routes to Desc.qq", {
  f <- desc(a ~ b, data = d)
  expect_length(f, 1L)
  expect_s3_class(f[[1]], c("Desc.qq", "Desc"))
  expect_identical(f[[1]]$meta$xname, "b")
  expect_identical(f[[1]]$meta$yname, "a")
  expect_equal(as.vector(f[[1]]$res$tab), as.vector(table(d$a, d$b)))
  expect_equal(f[[1]]$pair$nValid, 12L)
})

test_that("print.Desc.qq prints pair summary and table", {
  f <- desc(a ~ b, data = d)
  expect_output(expect_invisible(print(f[[1]])), "pairs: ")
  expect_output(print(f), "Chi-squared")
})

test_that("plot.Desc.qq draws without error", {
  local_null_device()
  f <- desc(a ~ b, data = d)
  expect_no_error(plot(f[[1]]))
})
