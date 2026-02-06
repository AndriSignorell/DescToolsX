# 
# test_that("one-sample wilcox is detected", {
#   df <- data.frame(x = rnorm(10))
#   res <- .parse_formula(x ~ 1, df)
#   
#   expect_equal(res$type, "one.sample")
#   expect_equal(res$method, "wilcox")
#   expect_length(res$x, 10)
# })
# 
# 
# test_that("two-sample wilcox works", {
#   df <- data.frame(
#     y = rnorm(20),
#     g = rep(c("A", "B"), each = 10)
#   )
#   
#   res <- .parse_formula(y ~ g, df)
#   
#   expect_equal(res$type, "two.sample")
#   expect_length(res$x, 10)
#   expect_length(res$y, 10)
# })
# 
# 
# test_that("kruskal n-sample works", {
#   df <- data.frame(
#     y = rnorm(30),
#     g = factor(rep(1:3, each = 10))
#   )
#   
#   res <- .parse_formula(y ~ g, df)
#   
#   expect_equal(res$type, "n.sample")
#   expect_equal(res$method, "kruskal")
# })
# 
# 
# test_that("friedman is detected", {
#   df <- data.frame(
#     y = rnorm(12),
#     trt = factor(rep(1:3, 4)),
#     block = factor(rep(1:4, each = 3))
#   )
#   
#   res <- .parse_formula(y ~ trt | block, df)
#   
#   expect_equal(res$type, "paired")
#   expect_equal(res$method, "friedman")
# })
# 
# 
# test_that("subset is respected", {
#   df <- data.frame(
#     y = rnorm(20),
#     g = rep(c("A", "B"), each = 10)
#   )
#   
#   res <- .parse_formula(y ~ g, df, subset = y > 0)
#   
#   expect_true(all(res$x > 0))
#   expect_true(all(res$y > 0))
# })
