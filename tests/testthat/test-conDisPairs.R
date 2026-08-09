x <- c(1, 2, 3, 1, 2)
y <- c(2, 1, 3, 2, 1)


test_that("the five counts partition all pairs", {

  # REGRESSION: condis_pairs_xy_cpp() returns only four names, so the R
  # side's z[c(...,"Ties_XY")] produced a fifth element NA under the name
  # NA - and sum() over the result was NA for every vector-mode call
  p <- conDisPairs(x, y)

  expect_named(p, c("C", "D", "Ties_X", "Ties_Y", "Ties_XY"))
  expect_equal(sum(p), choose(length(x), 2))

  # worked out by hand for this example
  expect_equal(unname(p[["C"]]), 4)
  expect_equal(unname(p[["D"]]), 4)
  expect_equal(unname(p[["Ties_XY"]]), 2)

  # table mode gives the same answer
  expect_equal(conDisPairs(table(x, y)), p)
})


test_that("an ordered factor is accepted and read by its level order", {

  xo <- ordered(c("low", "mid", "high", "low", "mid"),
                levels = c("low", "mid", "high"))
  yo <- ordered(c("mid", "low", "high", "mid", "low"),
                levels = c("low", "mid", "high"))

  expect_equal(conDisPairs(xo, yo), conDisPairs(x, y))

  # mixing the two types is fine
  expect_equal(conDisPairs(xo, y), conDisPairs(x, y))
})


test_that("an unordered factor is refused rather than silently ordered", {

  # as.integer() would impose the alphabetical order of the levels, and the
  # concordance counts would then be an artefact of the level names
  xf <- factor(c("low", "mid", "high", "low", "mid"))

  expect_error(conDisPairs(xf, y), "unordered factor")
  expect_error(conDisPairs(x, xf), "unordered factor")

  # and the message names the offending argument
  expect_error(conDisPairs(x, xf), "'y'")

  expect_error(conDisPairs(letters[1:5], y), "ordered factor")
})


test_that("the table mode checks its input", {

  # is.table() is TRUE for any number of dimensions
  a <- array(1, dim = c(2, 2, 2))
  expect_error(conDisPairs(as.table(a)), "two-dimensional")

  # NA used to slip past any(x < 0, na.rm = TRUE) and then abort inside
  # if (sum(x) < 2) with a message about the condition
  m <- matrix(c(1, 2, NA, 4), nrow = 2)
  expect_error(conDisPairs(m), "missing")

  expect_error(conDisPairs(matrix(c(1, -2, 3, 4), nrow = 2)), "non-negative")
  expect_error(conDisPairs(matrix(letters[1:4], nrow = 2)), "numeric")
  expect_error(conDisPairs(1:4), "contingency table")
})


test_that("too little data gives NA rather than an error", {

  na5 <- setNamesX(rep(NA_real_, 5),
                   c("C", "D", "Ties_X", "Ties_Y", "Ties_XY"))

  expect_equal(conDisPairs(1, 1), na5)
  expect_equal(conDisPairs(matrix(c(1, 0, 0, 0), nrow = 2)), na5)

  # NAs are removed pairwise, so this leaves one usable pair member
  expect_equal(conDisPairs(c(1, NA), c(1, 2)), na5)
})


test_that("Ties_XY is reconstructed, not left as an NA slot", {

  p <- conDisPairs(x, y)

  expect_false(anyNA(names(p)))
  expect_false(anyNA(p))
  expect_equal(names(p)[5], "Ties_XY")

  # the count now comes from the C++ itself, not from a reconstruction
  z <- DescToolsX:::condis_pairs_xy_cpp(x, y)
  expect_named(z, c("C", "D", "Ties_X", "Ties_Y", "Ties_XY"))
  expect_equal(unname(p[["Ties_XY"]]), unname(z[["Ties_XY"]]))

  # and the two modes agree on the TYPE, not only on the numbers: the
  # table core returns a List, which used to travel out as a list
  expect_type(conDisPairs(x, y), "double")
  expect_type(conDisPairs(table(x, y)), "double")

  # a case with ties in one variable only
  a <- c(1, 1, 2, 3)
  b <- c(1, 2, 3, 4)
  q <- conDisPairs(a, b)
  expect_equal(sum(q), choose(4, 2))
  expect_equal(unname(q[["Ties_X"]]), 1)
  expect_equal(unname(q[["Ties_XY"]]), 0)
})


test_that("both modes use the exclusive tie definition", {

  # REGRESSION: the table core built Ties_X and Ties_Y from the marginal
  # sums, which count a pair tied in BOTH variables once on each side. The
  # two entry points therefore reported different numbers for the same
  # data - here 14 counted pairs where there are 10 - and the excess was
  # exactly 2 * Ties_XY.
  p <- conDisPairs(x, y)
  q <- conDisPairs(table(x, y))

  expect_equal(q, p)
  expect_equal(sum(q), choose(length(x), 2))

  # the doubled cells are the ones that used to be counted twice
  expect_equal(unname(q[["Ties_XY"]]), 2)
  expect_equal(unname(q[["Ties_X"]]), 0)
  expect_equal(unname(q[["Ties_Y"]]), 0)

  # a table with ties in one margin only, where inclusive and exclusive
  # coincide - both cores must have agreed here even before
  a <- c(1, 1, 2, 3)
  b <- c(1, 2, 3, 4)
  expect_equal(conDisPairs(table(a, b)), conDisPairs(a, b))
})
