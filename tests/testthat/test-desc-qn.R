withr::local_options(list(DescToolsX.plotit = FALSE))

set.seed(1)
n  <- 80
x  <- rnorm(n)
y2 <- factor(ifelse(x + rnorm(n) > 0, "yes", "no"))
y3 <- cut(x + rnorm(n), 3, labels = c("lo", "mid", "hi"))

test_that(".descQN() with a binary y reproduces the reference statistics", {
  r <- .descQN(y2, x)
  expect_s3_class(r, "Desc.qn")
  expect_identical(r$k, 2L)
  expect_identical(r$lvls, c("no", "yes"))
  expect_equal(unname(r$kw$statistic),
               unname(kruskal.test(x ~ y2)$statistic))
  expect_equal(unname(r$tauB$r),
               cor(as.integer(y2), x, method = "kendall"))
  expect_equal(r$spearman$r, cor(as.integer(y2), x, method = "spearman"))
  expect_false(is.null(r$auc))
  expect_false(is.null(attr(r$eta2, "label")))
})

test_that("binary y: prevalence by quartile group and trend test", {
  r <- .descQN(y2, x)
  p <- r$prevTable
  expect_s3_class(p, "data.frame")
  expect_identical(nrow(p), 4L)
  expect_equal(sum(p$n), n)
  q1 <- x < quantile(x, 0.25)
  expect_equal(p$est[1], mean(y2[q1] == "yes"))
  expect_true(all(p$lci <= p$est & p$est <= p$uci))
  expect_s3_class(r$caTest, "htest")
})

test_that("custom breaks define the groups", {
  r <- .descQN(y2, x, breaks = 0)
  expect_identical(nrow(r$prevTable), 2L)
  expect_identical(r$prevTable$n, c(sum(x < 0), sum(x >= 0)))
})

test_that("more than two levels: conditional table, no AUC/trend test", {
  r <- .descQN(y3, x)
  expect_identical(r$k, 3L)
  expect_null(r$auc)
  expect_null(r$caTest)
  expect_false(is.data.frame(r$prevTable))
  expect_identical(dim(r$prevTable), c(3L, 4L))
})

test_that(".descQN() drops incomplete pairs and needs two levels", {
  yy <- replace(y2, 1, NA)
  xx <- replace(x, 2, NA)
  expect_length(.descQN(yy, xx)$xOk, n - 2L)
  expect_error(.descQN(factor(rep("a", 10)), rnorm(10)), "two distinct levels")
})

test_that(".normalizeBreaks() adds open ends only where needed", {
  b <- .normalizeBreaks(c(2, 4), x = 1:6)
  expect_equal(as.vector(b), c(-Inf, 2, 4, Inf))
  expect_length(attr(b, "labels"), 3L)
  expect_match(attr(b, "labels")[1], "^\\[")
  b <- .normalizeBreaks(c(0, 10), x = 1:6, right = TRUE)
  expect_equal(as.vector(b), c(0, 10))
  expect_match(attr(b, "labels"), "^\\(")
})

test_that("desc(factor ~ numeric) routes to Desc.qn and prints", {
  f <- desc(y2 ~ x, data = data.frame(y2, x))
  expect_s3_class(f[[1]], c("Desc.qn", "Desc"))
  for (v in 1:3)
    expect_output(expect_invisible(print(f[[1]], verbose = v)),
                  "Kruskal-Wallis", info = v)
  expect_output(print(f[[1]], verbose = 3), "Spearman")
  f3 <- desc(y3 ~ x, data = data.frame(y3, x))
  expect_output(print(f3[[1]]), "Conditional distribution")
})
