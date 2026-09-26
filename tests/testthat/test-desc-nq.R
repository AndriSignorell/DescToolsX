withr::local_options(list(DescToolsX.plotit = FALSE))

dnq <- data.frame(
  y = c(4.1, 5.2, 3.8, 6.0, 7.1, 6.6, 8.2, 7.9, 5.5, 9.1, 8.8, NA),
  g = factor(rep(c("a", "b", "c"), each = 4))
)

test_that(".descNQ matches kruskal.test and uses the complete n for eta^2", {
  r  <- desc(y ~ g, data = dnq)[["g"]]$res
  kw <- kruskal.test(y ~ g, data = dnq)
  expect_equal(unname(r$test$statistic), unname(kw$statistic))
  # 11 complete observations in 3 groups; the NA row used to count
  expect_equal(as.numeric(r$eta),
               max(0, (unname(kw$statistic) - 3 + 1) / (11 - 3)))
  expect_identical(colnames(r$tab), c("a", "b", "c"))
  expect_identical(nrow(r$tab), 8L)
})

test_that("print.Desc.nq shows both tests and warns about missing groups", {
  res <- desc(y ~ g, data = dnq)[["g"]]
  out <- capture.output(r <- print(res))
  expect_identical(r, res)
  expect_true(any(grepl("Kruskal-Wallis", out)))
  expect_true(any(grepl("Levene", out)))

  d2 <- dnq
  d2$g[2] <- NA
  res2 <- desc(y ~ g, data = d2)[["g"]]
  # desc.formula() passes the missing group on (na.pass), the pair summary
  # counts it
  expect_identical(res2$pair$nMissingGroups, 1L)
  # the note is printed by .printWarning() (dezent grau via cat), it is no
  # longer a warning condition; collect stdout and stderr, whichever it uses
  err <- character()
  out <- capture.output(err <- capture.output(print(res2), type = "message"))
  expect_true(any(grepl("Grouping variable contains 1 NAs", c(out, err),
                        fixed = TRUE)))
})

test_that(".eta2Kruskal clamps at zero and labels the size", {
  e <- .eta2Kruskal(H = 0.5, k = 3, n = 30)
  expect_equal(as.numeric(e), 0)
  expect_identical(attr(e, "label"), "negligible")
  expect_identical(attr(.eta2Kruskal(H = 10, k = 3, n = 30), "label"), "large")
  expect_identical(attr(.eta2Kruskal(H = 3, k = 3, n = 30), "label"), "small")
})

test_that("a group without any valid value is summarised as NA", {
  tab <- .buildSummaryTable(list(a = desc(c(1.5, 2.5, 3.5)),
                                 b = desc(c(NA_real_, NA_real_))))
  expect_identical(dim(tab), c(8L, 2L))
})

test_that("plot.Desc.nq draws and names an unknown plot type", {
  local_null_device()
  res <- desc(y ~ g, data = dnq)[["g"]]
  expect_no_error(plot(res))
  expect_warning(plot(res, which = 9), "which = 9")
})
