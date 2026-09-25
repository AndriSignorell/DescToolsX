lvl <- c("a", "b", "c")
tab <- as.table(matrix(c(10, 2, 1, 3, 12, 2, 0, 4, 9), 3,
                       dimnames = list(r1 = lvl, r2 = lvl)))

test_that("tables, matrices and numeric data frames are recognised", {
  expect_true(isConfusionTable(tab))
  expect_true(isConfusionTable(as.matrix(tab)))
  expect_true(isConfusionTable(as.data.frame.matrix(tab)))
  expect_true(isConfusionTable(unname(as.matrix(tab)), requireDimnames = FALSE))
})

test_that("anything that is not a 2-d table, matrix or data frame is refused", {
  expect_false(isConfusionTable(1:9))
  expect_false(isConfusionTable(list(a = 1, b = 2)))
  expect_false(isConfusionTable(table(1:2, 1:2, 1:2)))
  expect_false(isConfusionTable(array(1:8, c(2, 2, 2))))
})

test_that("non-square input is refused unless requireSquare = FALSE", {
  m <- matrix(1:6, 2, dimnames = list(c("a", "b"), c("a", "b", "c")))
  expect_false(isConfusionTable(m))
  expect_false(isConfusionTable(as.table(m)))
  expect_false(isConfusionTable(as.data.frame(m)))
  # levels must still match unless that check is switched off too
  expect_false(isConfusionTable(m, requireSquare = FALSE))
  expect_true(isConfusionTable(m, requireSquare = FALSE,
                               requireSameLevels = FALSE))
})

test_that("non-numeric content is refused", {
  expect_false(isConfusionTable(matrix(letters[1:4], 2)))
  df <- data.frame(a = c(1, 2), b = c("x", "y"))
  expect_false(isConfusionTable(df, requireDimnames = FALSE))
})

test_that("missing, infinite and negative cells are refused", {
  m <- as.matrix(tab)
  for (bad in c(NA, NaN, Inf, -1)) {
    mb <- m
    mb[2, 3] <- bad
    expect_false(isConfusionTable(mb), info = bad)
  }
})

test_that("dimnames: presence and matching levels", {
  m <- as.matrix(tab)
  expect_false(isConfusionTable(unname(m)))
  # data frames with automatic row names have no row names after as.matrix()
  df <- data.frame(a = 1:2, b = 3:4)
  expect_false(isConfusionTable(df))
  expect_true(isConfusionTable(df, requireDimnames = FALSE))
  # same set in a different order is accepted, a different set is not
  m2 <- m
  dimnames(m2) <- list(c("a", "b", "c"), c("c", "a", "b"))
  expect_true(isConfusionTable(m2))
  dimnames(m2) <- list(c("a", "b", "c"), c("a", "b", "z"))
  expect_false(isConfusionTable(m2))
  expect_true(isConfusionTable(m2, requireSameLevels = FALSE))
})

test_that("counts must be integer-like within integerTol", {
  m <- as.matrix(tab)
  m[1, 1] <- 10 + 1e-10
  expect_true(isConfusionTable(m))
  expect_false(isConfusionTable(m, integerTol = 0, acceptProportions = FALSE))
  m[1, 1] <- 10.5
  expect_false(isConfusionTable(m))
})

test_that("proportion tables are accepted only when asked and summing to 1", {
  p <- prop.table(tab)
  expect_true(isConfusionTable(p))
  expect_false(isConfusionTable(p, acceptProportions = FALSE))
  expect_false(isConfusionTable(p / 2))
})

test_that(".IsNonNegIntMatrix checks shape, sign and integrality", {
  expect_false(.IsNonNegIntMatrix(1:4))
  expect_false(.IsNonNegIntMatrix(matrix(letters[1:4], 2)))
  expect_false(.IsNonNegIntMatrix(matrix(1:6, 2)))
  expect_true(.IsNonNegIntMatrix(matrix(numeric(0), 0, 0)))
  expect_false(.IsNonNegIntMatrix(matrix(c(1, NA, 2, 3), 2)))
  expect_false(.IsNonNegIntMatrix(matrix(c(1, -1, 2, 3), 2)))
  expect_true(.IsNonNegIntMatrix(matrix(c(1, -1e-12, 2, 3), 2)))
  expect_true(.IsNonNegIntMatrix(matrix(1:4, 2)))
  expect_false(.IsNonNegIntMatrix(matrix(c(1, 1.5, 2, 3), 2)))
  # a single non-integer among large counts: all.equal() let it pass
  expect_false(.IsNonNegIntMatrix(matrix(c(1e8, 1e8, 1e8, 0.5), 2)))
})
