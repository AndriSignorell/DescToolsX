# all tests pass an explicit fmt so that they do not depend on the
# style objects abs.sty / num.sty / per.sty being defined in the workspace
FMT <- list(abs  = style(digits = 0),
            num  = style(digits = 1),
            per  = style(fmt = "%", digits = 1),
            pval = style(fmt = "*", naForm = "   "))

d.set <- data.frame(
  num  = c(1, 2, 3, 4, 5, 6, 7, 8),
  dich = c(0, 1, 1, 0, 1, 1, 0, 1),
  cat  = factor(c("a", "b", "c", "a", "b", "c", "a", "b")),
  g    = factor(rep(c("x", "y"), each = 4))
)


test_that("user supplied vnames are used verbatim", {

  # regression: default_vnames was TRUE in both branches, so the
  # "(= level)" suffix was appended even to user supplied names and the
  # else branch was dead code
  t1 <- tOne(d.set[, c("num", "dich")], d.set$g, TEST = NA, fmt = FMT,
             vnames = c("Score", "Flag"))

  expect_true("Flag" %in% unclass(t1)[, 1])
  expect_false(any(grepl("(= ", unclass(t1)[, 1], fixed = TRUE)))

  # without vnames the reported level is appended
  t2 <- tOne(d.set[, c("num", "dich")], d.set$g, TEST = NA, fmt = FMT)
  expect_true(any(grepl("dich (= 1)", unclass(t2)[, 1], fixed = TRUE)))

})


test_that("a matrix without column names gets one name per column", {

  # regression: vnames was the single string "Var1", so vnames[2] was NA
  m <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
  t1 <- tOne(m, TEST = NA, fmt = FMT)

  expect_false(anyNA(unclass(t1)[, 1]))

})


test_that("intref selects the reported level", {

  hi <- tOne(d.set["dich"], d.set$g, TEST = NA, fmt = FMT, intref = "high")
  lo <- tOne(d.set["dich"], d.set$g, TEST = NA, fmt = FMT, intref = "low")

  expect_true(any(grepl("= 1", unclass(hi)[, 1], fixed = TRUE)))
  expect_true(any(grepl("= 0", unclass(lo)[, 1], fixed = TRUE)))

  # "both" reports the variable as a categorical one, i.e. with all levels
  bo <- tOne(d.set["dich"], d.set$g, TEST = NA, fmt = FMT, intref = "both")
  expect_gt(nrow(bo), nrow(hi))

})


test_that("the total column uses the same denominator as the total counts", {

  # regression: cat_mat() computed the total percentages from
  # prop.table(table(x)), which includes observations with a missing group,
  # while the total counts are the row sums of table(x, g)
  # three levels, otherwise isDichotomous() routes the column to dich_mat()
  d <- data.frame(f = factor(c("a", "a", "b", "b", "c")),
                  g = factor(c("x", "x", "y", "y", NA)))

  t1 <- tOne(d["f"], d$g, TEST = NA, fmt = FMT, add.length = FALSE)
  tot <- unclass(t1)[, "total"]
  tot <- tot[tot != ""]

  num <- as.numeric(sub("^\\s*([0-9.]+).*", "\\1", tot))
  pct <- as.numeric(sub(".*\\(\\s*([0-9.]+).*", "\\1", tot))

  expect_equal(sum(num), 4)                 # the NA group is not counted
  expect_equal(pct, num / sum(num) * 100, tolerance = 1e-6)

})


test_that("a one-row table keeps its dimensions", {

  # regression: res[, -3] / res[, -ncol(res)] dropped to a vector whenever the
  # table had a single row - one dichotomous variable and add.length = FALSE
  t1 <- tOne(d.set["dich"], d.set$g, TEST = NA, fmt = FMT, add.length = FALSE)
  expect_true(is.matrix(unclass(t1)))
  expect_equal(nrow(unclass(t1)), 1L)
  expect_true("total" %in% colnames(unclass(t1)))

  t2 <- tOne(d.set["dich"], TEST = NA, fmt = FMT, add.length = FALSE)
  expect_true(is.matrix(unclass(t2)))

})


test_that("an unsupported column type does not break the table", {

  d <- data.frame(num = 1:6,
                  dat = as.Date("2020-01-01") + 0:5,
                  g   = factor(rep(c("x", "y"), 3)))

  expect_silent(t1 <- tOne(d[, c("num", "dat")], d$g, TEST = NA, fmt = FMT))
  expect_equal(ncol(unclass(t1)), 4)         # var + total + 2 groups
  expect_true("dat" %in% unclass(t1)[, 1])

})


test_that("the ungrouped table has no group column", {

  t1 <- tOne(d.set[, c("num", "cat")], TEST = NA, fmt = FMT)
  expect_equal(ncol(unclass(t1)), 2)         # var + total

  t2 <- tOne(d.set[, c("num", "cat")], d.set$g, TEST = NA, fmt = FMT)
  expect_equal(ncol(unclass(t2)), 4)         # var + total + 2 groups

  t3 <- tOne(d.set[, c("num", "cat")], d.set$g, TEST = NA, fmt = FMT, total = FALSE)
  expect_equal(ncol(unclass(t3)), 3)

})


test_that("TEST = NA drops the test column and the legend", {

  t1 <- tOne(d.set[, c("num", "cat")], d.set$g, TEST = NA, fmt = FMT)
  expect_null(attr(t1, "legend"))

  t2 <- tOne(d.set[, c("num", "cat")], d.set$g, fmt = FMT)
  expect_true(nzchar(attr(t2, "legend")))
  expect_equal(ncol(unclass(t2)), 5)         # ... plus the test column

})


test_that("fmt entries are merged by name, not by value", {

  # regression: fmt[!duplicated(fmt)] compared the VALUES, so a user entry
  # that happened to equal a default (or another user entry) silently
  # removed the other one
  sty <- style(digits = 0)
  t1 <- tOne(d.set[, c("num", "cat")], d.set$g, TEST = NA,
             fmt = list(abs = sty, num = sty,
                        per = style(fmt = "%", digits = 1),
                        pval = style(fmt = "*", naForm = "   ")))

  # num is formatted with 0 digits, so no decimal point in the numeric row
  numrow <- unclass(t1)[unclass(t1)[, 1] == "num", "total"]
  expect_false(grepl(".", numrow, fixed = TRUE))

})


test_that("print() returns its argument invisibly", {

  t1 <- tOne(d.set[, c("num", "cat")], d.set$g, TEST = NA, fmt = FMT)
  expect_output(res <- print(t1))
  expect_identical(res, t1)

})


test_that("subsetting keeps class and legend", {

  t1 <- tOne(d.set[, c("num", "cat")], d.set$g, fmt = FMT)
  sub <- t1[1:2, ]

  expect_s3_class(sub, "tOne")
  expect_equal(attr(sub, "legend"), attr(t1, "legend"))
  expect_equal(nrow(unclass(sub)), 2)

})
