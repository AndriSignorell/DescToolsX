
.m <- as.table(cbind(c(1768,946,115), c(807,1387,438), c(189,746,288), c(47,53,16)))

test_that("uncertCoef returns a single numeric (symmetric, no CI)", {
  res <- uncertCoef(.m)
  expect_length(res, 1); expect_true(is.numeric(res))
})

test_that("uncertCoef result is in [0, 1]", {
  res <- uncertCoef(.m)
  expect_gte(res, 0); expect_lte(res, 1)
})

test_that("uncertCoef direction = 'row' and 'column' give different values", {
  r <- uncertCoef(.m, direction = "row")
  c <- uncertCoef(.m, direction = "column")
  expect_false(isTRUE(all.equal(r, c)))
})

test_that("uncertCoef is 0 for an independent table (all cells equal)", {
  tab <- as.table(matrix(rep(25,4), nrow=2))
  expect_equal(uncertCoef(tab), 0, tolerance=1e-6)
})

test_that("uncertCoef symmetric is the harmonic mean of row and column", {
  # sym = 2*MI/(H(X)+H(Y)), row = MI/H(X), col = MI/H(Y)
  # => sym = 2*row*col / (row+col)  (harmonic mean)
  r   <- uncertCoef(.m, direction = "row")
  col <- uncertCoef(.m, direction = "column")
  sym <- uncertCoef(.m, direction = "symmetric")
  expect_equal(sym, 2 * r * col / (r + col), tolerance = 1e-10)
  expect_true(sym >= 0 && sym <= 1)
})

test_that("uncertCoef conf.level returns named vector est/lci/uci", {
  res <- uncertCoef(.m, conf.level=0.95)
  expect_length(res, 3)
  expect_named(res, c("est","lci","uci"))
})

test_that("uncertCoef CI: lci < est < uci", {
  res <- uncertCoef(.m, conf.level=0.95)
  expect_lt(res["lci"], res["est"])
  expect_gt(res["uci"], res["est"])
})

test_that("uncertCoef accepts two vectors", {
  x <- factor(c("A","A","B","B"))
  y <- factor(c("X","Y","X","Y"))
  res <- uncertCoef(x, y)
  expect_gte(res, 0); expect_lte(res, 1)
})

