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




# The 2x5 table from the SAS PROC FREQ documentation, the same one used by
# somersDelta() and stuartTauC().
sasTab <- as.table(rbind(
  c(26, 26, 23, 18,  9),
  c( 6,  7,  9, 14, 23)
))


test_that("the table branch agrees with cor(method='spearman')", {
  
  # Both routes use midranks, so the agreement is exact, not approximate -
  # this is the claim the examples make.
  long <- data.frame(
    row = rep(rep(1:2, times = 5), times = as.vector(sasTab)),
    col = rep(rep(1:5, each  = 2), times = as.vector(sasTab))
  )
  
  expect_equal(
    spearmanCor(sasTab),
    cor(long$row, long$col, method = "spearman")
  )
  
  expect_equal(spearmanCor(sasTab), 0.3770608813, tolerance = 1e-9)
})


test_that("the confidence interval follows the z-transformation", {
  
  res <- spearmanCor(sasTab, conf.level = 0.95)
  
  expect_named(res, c("est", "lci", "uci"))
  expect_equal(unname(res),
               c(0.3770608813, 0.2361592722, 0.5024329224),
               tolerance = 1e-9)
  
  expect_true(res[["lci"]] <= res[["est"]])
  expect_true(res[["est"]] <= res[["uci"]])
})


test_that("table and vector interface give the same interval", {
  
  long <- data.frame(
    row = rep(rep(1:2, times = 5), times = as.vector(sasTab)),
    col = rep(rep(1:5, each  = 2), times = as.vector(sasTab))
  )
  
  expect_equal(
    spearmanCor(sasTab, conf.level = 0.95),
    spearmanCor(long$row, long$col, conf.level = 0.95)
  )
})


test_that("sides is applied and names the finite side", {
  
  # sides used to be accepted and then ignored entirely.
  two   <- spearmanCor(sasTab, conf.level = 0.95)
  left  <- spearmanCor(sasTab, conf.level = 0.95, sides = "left")
  right <- spearmanCor(sasTab, conf.level = 0.95, sides = "right")
  
  expect_identical(left[["uci"]], 1)
  expect_identical(right[["lci"]], -1)
  
  # the full alpha sits on the finite side, so the one-sided bound is inside
  # the two-sided one
  expect_true(left[["lci"]] > two[["lci"]])
  expect_true(right[["uci"]] < two[["uci"]])
  
  n <- sum(sasTab)
  expect_equal(
    left[["lci"]],
    tanh(atanh(0.3770608813) - qnorm(0.95) / sqrt(n - 3)),
    tolerance = 1e-9
  )
})


test_that("perfect correlation does not blow up the transformation", {
  
  res <- spearmanCor(1:10, 1:10, conf.level = 0.95)
  expect_equal(unname(res), c(1, 1, 1))
  
  res <- spearmanCor(1:10, 10:1, conf.level = 0.95)
  expect_equal(unname(res), c(-1, -1, -1))
})


test_that("ordered factors are accepted", {
  
  a <- ordered(c("lo", "lo", "mid", "hi", "hi"),
               levels = c("lo", "mid", "hi"))
  b <- c(1, 2, 2, 4, 5)
  
  expect_equal(spearmanCor(a, b),
               cor(as.numeric(a), b, method = "spearman"))
})


test_that("na.rm removes incomplete pairs", {
  
  a <- c(1, 2, 3, 4, NA)
  b <- c(2, 1, 4, 3,  5)
  
  expect_true(is.na(spearmanCor(a, b)))
  expect_equal(spearmanCor(a, b, na.rm = TRUE),
               cor(a[1:4], b[1:4], method = "spearman"))
})


test_that("a non-table input is refused instead of failing on dim()", {
  
  # regression: the guard sat AFTER as.matrix(), which turns a vector into an
  # n x 1 matrix - spearmanCor(1:10) then ran through the table branch and
  # returned NA instead of refusing the input
  expect_error(spearmanCor(1:10), "two-dimensional")
  expect_error(spearmanCor(c(a = 1, b = 2, c = 3)), "two-dimensional")
  expect_error(spearmanCor(as.table(c(a = 4, b = 6))), "two-dimensional")
  expect_error(spearmanCor(array(1:8, dim = c(2, 2, 2))), "two-dimensional")
  expect_error(spearmanCor(matrix(letters[1:4], 2)), "two-dimensional")
  expect_error(spearmanCor(matrix(c(1, NA, 2, 3), 2)), "missing")
  expect_error(spearmanCor(matrix(c(1, -1, 2, 3), 2)), "negative")
  expect_error(spearmanCor(sasTab, conf.level = 0), "conf.level")
  expect_error(spearmanCor(sasTab, conf.level = NULL), "conf.level")
})




test_that("spearmanCor() reports NA where the z-transformation is undefined", {
  
  # atanh(1) is infinite: the interval used to collapse to (rho, rho) and
  # rule out every value below 1
  perfect <- data.frame(a = 1:6, b = 1:6)
  expect_equal(unname(spearmanCor(perfect$a, perfect$b)), 1)
  
  expect_warning(ci <- spearmanCor(perfect$a, perfect$b, conf.level = 0.95),
                 "perfect correlation")
  expect_equal(unname(ci[["est"]]), 1)
  expect_true(is.na(ci[["lci"]]))
  expect_true(is.na(ci[["uci"]]))
  
  # n <= 3: Var(atanh(rho)) ~ 1/(n-3) is infinite at 3 and negative below.
  # (-1, 1) looked like a computed answer and was merely the whole range.
  x <- c(1, 2, 3); y <- c(2, 1, 3)
  expect_warning(ci <- spearmanCor(x, y, conf.level = 0.95),
                 "3 observations")
  expect_true(is.na(ci[["lci"]]))
  expect_equal(unname(ci[["est"]]), spearmanCor(x, y))
})


test_that("spearmanCor() refuses a one-sided interval below conf.level 0.5", {
  
  set.seed(1)
  x <- rnorm(40); y <- x + rnorm(40)
  
  expect_error(spearmanCor(x, y, conf.level = 0.4, sides = "left"), "0.5")
  expect_error(spearmanCor(x, y, conf.level = 0.5, sides = "right"), "0.5")
  expect_silent(spearmanCor(x, y, conf.level = 0.4))
})


test_that("spearmanCor() closes the open side at the range boundary", {
  
  set.seed(2)
  x <- rnorm(40); y <- x + rnorm(40)
  
  two   <- spearmanCor(x, y, conf.level = 0.95)
  left  <- spearmanCor(x, y, conf.level = 0.95, sides = "left")
  right <- spearmanCor(x, y, conf.level = 0.95, sides = "right")
  
  expect_equal(left[["uci"]], 1)
  expect_equal(right[["lci"]], -1)
  expect_equal(unname(left[["est"]]), unname(two[["est"]]))
  
  expect_gte(left[["lci"]],  two[["lci"]])
  expect_lte(right[["uci"]], two[["uci"]])
  
  # NA-Grenzen ueberleben die Seitenbehandlung
  suppressWarnings(
    ci <- spearmanCor(c(1, 2, 3), c(2, 1, 3), conf.level = 0.95,
                      sides = "left"))
  expect_true(is.na(ci[["lci"]]))
  expect_equal(ci[["uci"]], 1)
})


