withr::local_options(list(DescToolsX.plotit = FALSE))

xnum <- c(3.1, 4.7, 2.2, 5.9, 4.4, 3.8, 6.1, 2.9, 5.0, 4.2, 3.3, 5.5, 4.9, 3.0)

test_that("plotit = FALSE draws nothing", {
  local_null_device()
  expect_identical(count_plot_new(capture.output(print(desc(xnum)))), 0L)
})

test_that("a container plots each element exactly once", {
  local_null_device()
  d <- desc(xnum, plotit = TRUE)
  k <- count_plot_new(capture.output(print(d)))
  expect_gt(k, 0L)
  ctr <- structure(list(a = d, b = d), class = c("Desc", "list"))
  expect_identical(count_plot_new(capture.output(print(ctr))), 2L * k)
})

test_that("desc(factor ~ factor) plots the pair once, not the inner table", {
  withr::local_options(list(DescToolsX.plotit = TRUE))
  local_null_device()
  d <- data.frame(
    a = factor(c("x", "y", "x", "y", "x", "x", "y", "y", "x", "y")),
    b = factor(c("u", "u", "v", "v", "u", "v", "u", "v", "u", "v"))
  )
  f <- desc(a ~ b, data = d)
  k <- count_plot_new(plot(f[[1]]))
  expect_identical(count_plot_new(capture.output(print(f))), k)
})
