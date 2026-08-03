# Example table: 2x5 from SAS docs
.tab <- as.table(rbind(c(26,26,23,18,9), c(6,7,9,14,23)))

test_that("gkGamma returns a single numeric", {
  g <- gkGamma(.tab)
  expect_length(g, 1)
  expect_true(is.numeric(g))
})

test_that("gkGamma result is in [-1, 1]", {
  g <- gkGamma(.tab)
  expect_gte(g, -1); expect_lte(g, 1)
})

test_that("gkGamma is positive for a positively associated table", {
  tab_pos <- as.table(matrix(c(40,10,10,40), nrow=2))
  expect_gt(gkGamma(tab_pos), 0)
})

test_that("gkGamma is negative for a negatively associated table", {
  tab_neg <- as.table(matrix(c(10,40,40,10), nrow=2))
  expect_lt(gkGamma(tab_neg), 0)
})

test_that("gkGamma ≈ 0 for independent table", {
  tab_ind <- as.table(matrix(rep(25,4), nrow=2))
  expect_lt(abs(gkGamma(tab_ind)), 0.05)
})

test_that("gkGamma is symmetric: gkGamma(x,y) == gkGamma(y,x)", {
  x <- c(1,2,2,3,3,3)
  y <- c(2,1,3,2,3,1)
  expect_equal(gkGamma(x, y), gkGamma(y, x))
})

test_that("gkGamma conf.level returns named vector", {
  res <- gkGamma(.tab, conf.level = 0.95)
  expect_length(res, 3)
  expect_true(!is.null(names(res)))
})

test_that("gkGamma accepts two vectors directly", {
  x <- c(1,2,2,3); y <- c(1,1,2,3)
  g <- gkGamma(x, y)
  expect_length(g, 1)
})



test_that("gkGamma forwards its dots and matches ordAssocs", {
  
  tab <- as.table(rbind(c(26, 26, 23, 18, 9), c(6, 7, 9, 14, 23)))
  
  expect_equal(gkGamma(tab), unname(ordAssocs(tab, which = "gamma")$gamma))
  
  g <- gkGamma(tab, conf.level = 0.95)
  expect_named(g, c("est", "lci", "uci"))
})

