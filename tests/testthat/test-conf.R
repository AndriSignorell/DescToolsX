# helper: simple 2-class setup
.pred2 <- factor(c("A","A","B","B","A","B","A","B"))
.ref2  <- factor(c("A","B","A","B","A","B","B","A"))

test_that("conf returns an object of class 'Conf'", {
  expect_s3_class(conf(.pred2, .ref2), "Conf")
})

test_that("conf result contains required components", {
  res <- conf(.pred2, .ref2)
  expect_true(all(c("table","acc","n","kappa","byclass") %in% names(res)))
})

test_that("conf accuracy is in [0, 1]", {
  res <- conf(.pred2, .ref2)
  expect_gte(res$acc, 0)
  expect_lte(res$acc, 1)
})

test_that("conf accuracy CI: lci <= acc <= uci", {
  res <- conf(.pred2, .ref2)
  expect_lte(res$acc.lci, res$acc)
  expect_gte(res$acc.uci, res$acc)
})

test_that("conf n equals total number of observations", {
  res <- conf(.pred2, .ref2)
  expect_equal(res$n, length(.pred2))
})

test_that("conf kappa is in [-1, 1]", {
  res <- conf(.pred2, .ref2)
  expect_gte(res$kappa, -1)
  expect_lte(res$kappa,  1)
})

test_that("conf perfect prediction gives accuracy 1", {
  x <- factor(c("A","A","B","B"))
  res <- conf(x, x)
  expect_equal(res$acc, 1)
})

test_that("conf byclass contains expected row metrics", {
  res <- conf(.pred2, .ref2, pos = "A")
  expected_rows <- c("sens","spec","ppv","npv")
  expect_true(all(expected_rows %in% rownames(res$byclass)))
})

test_that("conf.table accepts a table directly", {
  tab <- table(Pred = .pred2, Ref = .ref2)
  res <- conf(tab, pos = "A")
  expect_s3_class(res, "Conf")
})

test_that("conf multiclass returns byclass with one column per class", {
  pred <- factor(c("A","B","C","A","B","C","A","B","C"))
  ref  <- factor(c("A","A","C","B","B","C","A","C","B"))
  res  <- conf(pred, ref)
  expect_equal(ncol(res$byclass), 3L)
})

test_that("conf na.rm = TRUE handles missing values", {
  pred <- factor(c("A","B",NA,"A","B"))
  ref  <- factor(c("A","B","A","B","A"))
  expect_s3_class(conf(pred, ref, na.rm = TRUE), "Conf")
})

test_that("sensX() extracts sensitivity from conf object", {
  res <- sensX(.pred2, .ref2, pos = "A")
  expect_gte(res, 0)
  expect_lte(res, 1)
})

test_that("specX() extracts specificity from conf object", {
  res <- specX(.pred2, .ref2, pos = "A")
  expect_gte(res, 0)
  expect_lte(res, 1)
})



test_that("conf() survives class labels that are substrings of each other", {
  
  # "A" is a substring of "AB": grep(pos, ..., fixed = TRUE) matched both
  # and collapsed the 2x2 table to 1x1 without any error
  tab <- as.table(matrix(c(30, 5, 8, 27), nrow = 2,
                         dimnames = list(c("A", "AB"), c("A", "AB"))))
  
  res <- conf(tab, pos = "A")
  
  expect_equal(dim(res$table), c(2L, 2L))
  expect_equal(res$n, 70)
  expect_equal(res$diag, 57)
  expect_equal(unname(res$acc), 57 / 70)
})


test_that("conf() rejects an unknown positive class", {
  tab <- as.table(matrix(c(30, 5, 8, 27), nrow = 2,
                         dimnames = list(c("no", "yes"), c("no", "yes"))))
  expect_error(conf(tab, pos = "maybe"), "class labels")
})


test_that("multiclass one-vs-all collapsing is label-exact", {
  
  lbl <- c("1", "10", "11")
  tab <- as.table(matrix(c(20, 3, 2,
                           4, 18, 5,
                           1, 6, 21), nrow = 3, byrow = TRUE,
                         dimnames = list(lbl, lbl)))
  
  res <- conf(tab)
  
  # sensitivity of class "1" is 20 / (20 + 4 + 1)
  expect_equal(unname(res$byclass["sens", "1"]), 20 / 25)
  expect_equal(sum(res$byclass["prev", ]), 1)
})



