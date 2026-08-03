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


test_that("lambda survives a non-square table with a confidence interval", {
  
  # L.col was allocated with ncol elements but indexed by row (and L.row
  # the other way round), so a matrix - which does not grow - aborted with
  # "subscript out of bounds". The documented example is 3x4 and uses
  # direction = "symmetric", which reaches neither branch.
  m <- as.table(cbind(c(1768, 946, 115), c(807, 1387, 438),
                      c(189, 746, 288), c(47, 53, 16)))
  dimnames(m) <- list(paste("A", 1:3), paste("B", 1:4))
  
  expect_silent(a <- lambda(m, direction = "column", conf.level = 0.95))
  expect_silent(b <- lambda(m, direction = "row", conf.level = 0.95))
  
  for (r in list(a, b)) {
    expect_named(r, c("est", "lci", "uci"))
    expect_gte(r[["lci"]], 0)
    expect_lte(r[["uci"]], 1)
    expect_lte(r[["lci"]], r[["est"]])
    expect_gte(r[["uci"]], r[["est"]])
  }
  
  # transposed table: row and column swap roles
  expect_equal(unname(lambda(t(m), direction = "row")),
               unname(lambda(m, direction = "column")))
})


test_that("lambda reports the open side at the range boundary", {
  
  m <- as.table(cbind(c(1768, 946, 115), c(807, 1387, 438),
                      c(189, 746, 288), c(47, 53, 16)))
  
  left  <- lambda(m, conf.level = 0.95, sides = "left")
  right <- lambda(m, conf.level = 0.95, sides = "right")
  
  # lambda lies in [0, 1] and the two-sided interval is already clamped
  # to it, so +/-Inf claimed values the measure cannot take
  expect_equal(unname(left[["uci"]]), 1)
  expect_equal(unname(right[["lci"]]), 0)
  expect_true(is.finite(left[["lci"]]))
  expect_true(is.finite(right[["uci"]]))
})

