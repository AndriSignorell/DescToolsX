.tab_lambda <- as.table(rbind(c(26,26,23,18,9), c(6,7,9,14,23)))

test_that("lambda returns a single numeric (symmetric, no CI)", {
  res <- lambda(.tab_lambda)
  expect_length(res, 1)
  expect_true(is.numeric(res))
})

test_that("lambda result is in [0, 1]", {
  res <- lambda(.tab_lambda)
  expect_gte(res, 0); expect_lte(res, 1)
})

test_that("lambda direction = 'row' and 'column' can differ", {
  r <- lambda(.tab_lambda, direction = "row")
  c <- lambda(.tab_lambda, direction = "column")
  expect_false(isTRUE(all.equal(r, c)))
})

test_that("lambda is 0 for a perfectly independent table", {
  # All row maxima equal column max → no predictive gain
  tab <- as.table(matrix(rep(25, 4), nrow=2,
                         dimnames=list(c("A","B"), c("X","Y"))))
  # symmetric direction on balanced table
  res <- lambda(tab, direction = "symmetric")
  expect_equal(res, 0, tolerance = 1e-8)
})

test_that("lambda conf.level returns a 3-element named vector", {
  res <- lambda(.tab_lambda, conf.level = 0.95)
  expect_length(res, 3)
})

test_that("lambda CI: lower <= estimate <= upper", {
  res <- lambda(.tab_lambda, conf.level = 0.95, direction = "column")
  expect_lte(res[2], res[1])
  expect_gte(res[3], res[1])
})

test_that("lambda accepts two vectors", {
  x <- factor(c("A","A","B","B"))
  y <- factor(c("X","Y","X","Y"))
  res <- lambda(x, y)
  expect_length(res, 1)
})
