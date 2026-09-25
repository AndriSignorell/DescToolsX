withr::local_options(list(DescToolsX.plotit = FALSE))

set.seed(21)
dnn <- data.frame(x = rnorm(40, 10, 2))
dnn$y <- 3 + 0.8 * dnn$x + rnorm(40)
dnn$y[5] <- NA

test_that(".descNN reproduces the reference statistics", {
  r  <- desc(y ~ x, data = dnn)[["x"]]$res
  ok <- complete.cases(dnn)
  fit <- lm(y ~ x, data = dnn[ok, ])
  expect_identical(r$nTotal, 40L)
  expect_identical(r$nValid, 39L)
  expect_identical(r$nMiss, 1L)
  expect_equal(r$pearson$r, cor(dnn$x[ok], dnn$y[ok]))
  expect_equal(r$spearman$r, cor(dnn$x[ok], dnn$y[ok], method = "spearman"))
  expect_equal(r$pearson$ci, corCI(r$pearson$r, n = 39, conf.level = 0.95))
  expect_equal(r$lm$slope$est, unname(coef(fit)[2]))
  expect_equal(c(r$lm$slope$lci, r$lm$slope$uci), unname(confint(fit)[2, ]))
  expect_equal(r$lm$r2, summary(fit)$r.squared)
  expect_s3_class(r$shapiro, "htest")
  expect_equal(r$cookMax, max(cooks.distance(fit)))
})

test_that("the regression is the response on the predictor, not the reverse", {
  # desc.formula() passes (response, predictor); .descNN() took (x, y)
  # and so regressed x on y
  r <- desc(y ~ x, data = dnn)[["x"]]$res
  expect_equal(r$lm$slope$est, unname(coef(lm(y ~ x, data = dnn))[2]))
  expect_false(isTRUE(all.equal(r$lm$slope$est,
                                unname(coef(lm(x ~ y, data = dnn))[2]))))
})

test_that("print.Desc.nn honours verbose from the call and from desc()", {
  res <- desc(y ~ x, data = dnn)[["x"]]
  out1 <- capture.output(print(res, verbose = 1))
  out3 <- capture.output(print(res, verbose = 3))
  expect_false(any(grepl("Residual SE", out1)))
  expect_true(any(grepl("Breusch-Pagan", out3)))
  # verbose given to desc() is stored in meta and used by print()
  out <- capture.output(print(desc(y ~ x, data = dnn, verbose = 3)))
  expect_true(any(grepl("Breusch-Pagan", out)))
  expect_output(expect_invisible(print(res)), "Pearson  r:")
})

test_that("Shapiro-Wilk is skipped outside 3..5000 observations", {
  set.seed(22)
  x <- rnorm(5001)
  r <- .descNN(y = x + rnorm(5001), x = x)
  expect_null(r$shapiro)
  expect_output(.printNN(r, verbose = 2), "skipped")
})

test_that("stars and effect size labels follow their thresholds", {
  expect_identical(vapply(c(NA, 0.0005, 0.005, 0.02, 0.07, 0.5), .stars, ""),
                   c("", "***", " **", "  *", "  .", "   "))
  expect_identical(vapply(c(NA, -0.6, 0.35, 0.15, 0.05), .rLabel, ""),
                   c("", "large", "moderate", "small", "negligible"))
})

test_that("plot.Desc.nn draws and names an unknown plot type", {
  local_null_device()
  res <- desc(y ~ x, data = dnn)[["x"]]
  expect_no_error(plot(res))
  expect_warning(plot(res, which = 7), "which = 7")
})
