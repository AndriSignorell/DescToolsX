withr::local_options(list(DescToolsX.plotit = FALSE))

tab <- as.table(matrix(c(12, 5, 7, 9), 2,
                       dimnames = list(a = c("a1", "a2"), b = c("b1", "b2"))))

m <- matrix(c(153, 153, 167, 123, 108, 109, 89, 122, 167),
            nrow = 3, byrow = TRUE,
            dimnames = list(c("Brent", "Camden", "Westminster"),
                            c("Allanah", "Maria", "Rhonda")))

test_that("2x2 tables get the 2x2 tests, matching stats::", {
  d <- desc(tab)
  expect_s3_class(d, c("Desc.table", "Desc"))
  expect_identical(d$ttype, "t2x2")
  expect_equal(d$n, 33)
  expect_identical(d$dim, c(2L, 2L))

  ref  <- chisq.test(tab, correct = FALSE)
  refc <- chisq.test(tab, correct = TRUE)
  expect_equal(unname(d$chisq.test$statistic), unname(ref$statistic))
  expect_equal(d$chisq.test$p.value, ref$p.value)
  expect_equal(unname(d$chisq.test$parameter), 1)
  expect_equal(as.vector(d$chisq.test$expected), as.vector(ref$expected))
  expect_equal(unname(d$chisq.test.cont$statistic), unname(refc$statistic))
  expect_equal(d$fisher.test$p.value, fisher.test(tab)$p.value)
  expect_equal(d$mcnemar.test$p.value, mcnemar.test(tab)$p.value)
  expect_false(is.null(d$or))
  expect_true(d$approx.ok)
  expect_false(desc(as.table(matrix(c(1, 2, 3, 4), 2)))$approx.ok)
})

test_that("r x c matrices dispatch to desc.table", {
  d <- desc(m)
  expect_s3_class(d, "Desc.table")
  expect_identical(d$ttype, "trxc")
  ref <- chisq.test(m)
  expect_equal(unname(d$chisq.test$statistic), unname(ref$statistic))
  expect_equal(unname(d$chisq.test$parameter), 4)
  expect_null(d$fisher.test)
  expect_null(d$or)
  expect_false(is.null(d$loglik.chisq.test))
  expect_false(is.null(d$mh.test))
  expect_no_error(desc(m, prop = "cols"))
})

test_that("1-dim tables are tested against the uniform distribution", {
  t1 <- table(c(rep("a", 10), rep("b", 5), rep("c", 3)))
  d  <- desc(t1)
  expect_identical(d$ttype, "t1dim")
  ref <- chisq.test(c(10, 5, 3))
  expect_equal(unname(d$chisq.test$statistic), unname(ref$statistic))
  expect_equal(unname(d$chisq.test$parameter), 2)
  expect_equal(d$chisq.test$p.value, ref$p.value)
  expect_output(expect_invisible(print(d)), "1-dim uniform")
})

test_that("n-dim tables test mutual independence like summary.table()", {
  d   <- desc(Titanic)
  ref <- summary(Titanic)
  expect_identical(d$ttype, "tndim")
  expect_equal(unname(d$chisq.test$statistic), unname(ref$statistic))
  expect_equal(unname(d$chisq.test$parameter), unname(ref$parameter))
  expect_equal(d$chisq.test$p.value, ref$p.value)
  expect_null(d$perctab)
  expect_output(expect_invisible(print(d)), "4-dim table")
  expect_s3_class(desc(array(1:8, c(2, 2, 2))), "Desc.table")
})

test_that("verbose is stored and every level prints", {
  for (v in 1:3) {
    dm <- desc(m, verbose = v)
    dt <- desc(tab, verbose = v)
    expect_equal(dm$verbose, v)
    expect_output(expect_invisible(print(dm)), "Chi-squared", info = v)
    expect_output(print(dt), "Fisher", info = v)
  }
})

test_that(".chisqIndependence() applies Yates only to 2x2", {
  expect_identical(.chisqIndependence(m, correct = TRUE)$statistic,
                   .chisqIndependence(m, correct = FALSE)$statistic)
  expect_match(.chisqIndependence(tab, correct = TRUE)$method, "Yates")
})


test_that("the conf. level legend accompanies every table of intervals", {
  # 2x2: the estimates table with intervals appears from verbose = 2 on;
  # its legend used to be unreachable
  out1 <- capture.output(print(desc(tab, verbose = 1)))
  out2 <- capture.output(print(desc(tab, verbose = 2)))
  expect_false(any(grepl("conf. level", out1, fixed = TRUE)))
  expect_true(any(grepl("95% conf. level", out2, fixed = TRUE)))
  # r x c: intervals only in the full table of verbose = 3
  expect_false(any(grepl("conf. level",
                         capture.output(print(desc(m, verbose = 2))),
                         fixed = TRUE)))
  expect_true(any(grepl("conf. level",
                        capture.output(print(desc(m, verbose = 3))),
                        fixed = TRUE)))
})
