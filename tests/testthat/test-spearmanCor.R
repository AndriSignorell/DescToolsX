test_that("spearmanCor returns a value in [-1, 1] for two vectors", {
  x <- c(1,2,3,4,5); y <- c(5,4,3,2,1)
  res <- spearmanCor(x, y)
  expect_length(res, 1); expect_gte(res, -1); expect_lte(res, 1)
})

test_that("spearmanCor = -1 for perfectly anti-ranked vectors", {
  x <- 1:10; y <- 10:1
  expect_equal(spearmanCor(x, y), -1, tolerance = 1e-10)
})

test_that("spearmanCor = 1 for perfectly ranked vectors", {
  x <- 1:10
  expect_equal(spearmanCor(x, x), 1, tolerance = 1e-10)
})

test_that("spearmanCor matches cor(method='spearman') for vectors", {
  set.seed(1)
  x <- rnorm(50); y <- rnorm(50)
  expect_equal(spearmanCor(x, y), cor(x, y, method="spearman"),
               tolerance = 1e-10)
})

test_that("spearmanCor with conf.level returns named vector est/lci/uci", {
  x <- 1:20; y <- rank(rnorm(20))
  res <- spearmanCor(x, y, conf.level = 0.95)
  expect_length(res, 3)
  expect_named(res, c("est","lci","uci"))
})

test_that("spearmanCor CI: lci <= est <= uci", {
  set.seed(5)
  x <- rnorm(80); y <- x + rnorm(80)
  res <- spearmanCor(x, y, conf.level = 0.95)
  expect_lte(res["lci"], res["est"])
  expect_gte(res["uci"], res["est"])
})

test_that("spearmanCor na.rm = TRUE handles NA pairs", {
  x <- c(1, 2, NA, 4, 5)
  y <- c(5, 4, 3,  2, 1)
  expect_equal(spearmanCor(x, y, na.rm = TRUE),
               spearmanCor(c(1,2,4,5), c(5,4,2,1)))
})

test_that("spearmanCor accepts a frequency table", {
  pain <- as.table(matrix(c(26,6,26,7,23, 9,18,14,9,23),
                           ncol=5, byrow=TRUE))
  res <- spearmanCor(pain)
  expect_gte(res, -1); expect_lte(res, 1)
})
