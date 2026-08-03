
tab <- as.table(apply(HairEyeColor, c(1, 2), sum))


test_that(".addPercTableMargins() divides by the grand total, not by a corner", {

  tt <- list(freq = tab, perc = proportions(tab),
             p.row = proportions(tab, 1), p.col = proportions(tab, 2))
  n <- sum(tab)

  # margins = 1 only: the corner of the marginalised frequency table is the
  # total of the LAST COLUMN (64 for HairEyeColor), not the grand total 592.
  # The sum row of the percentages used to read 343.8% / 335.9% / ... .
  r1 <- .addPercTableMargins(tt, 1L)
  expect_equal(unname(r1$perc[nrow(r1$perc), ]), unname(colSums(tab) / n))
  expect_equal(sum(r1$perc[nrow(r1$perc), ]), 1)

  # margins = 2 only: the corner is the total of the last ROW (127)
  r2 <- .addPercTableMargins(tt, 2L)
  expect_equal(unname(r2$perc[, ncol(r2$perc)]), unname(rowSums(tab) / n))
  expect_equal(sum(r2$perc[, ncol(r2$perc)]), 1)

  # both margins: this case was already correct
  r12 <- .addPercTableMargins(tt, c(1L, 2L))
  expect_equal(unname(r12$perc[nrow(r12$perc), -ncol(r12$perc)]),
               unname(colSums(tab) / n))
  expect_equal(unname(r12$perc[-nrow(r12$perc), ncol(r12$perc)]),
               unname(rowSums(tab) / n))
  expect_equal(r12$perc[nrow(r12$perc), ncol(r12$perc)], 1)
})


test_that(".addPercTableMargins() puts the marginal distribution into p.row/p.col", {

  tt <- list(freq = tab, p.row = proportions(tab, 1))
  n <- sum(tab)

  r <- .addPercTableMargins(tt, c(1L, 2L))
  # the sum row is the column marginal distribution, the sum column the row
  # marginal distribution -- both from the frequency table
  expect_equal(unname(r$p.row[nrow(r$p.row), -ncol(r$p.row)]),
               unname(colSums(tab) / n))
  expect_equal(unname(r$p.row[-nrow(r$p.row), ncol(r$p.row)]),
               unname(rowSums(tab) / n))
  # and the interior is untouched
  expect_equal(unname(r$p.row[1:nrow(tab), 1:ncol(tab)]),
               unname(proportions(tab, 1)))
})


test_that(".addPercTableMargins() falls back to plain sums without freq", {

  tt <- list(p.row = proportions(tab, 1))
  r <- .addPercTableMargins(tt, 2L)
  # rows of p.row sum to 1
  expect_equal(unname(r$p.row[, ncol(r$p.row)]), rep(1, nrow(tab)))
})


test_that("percTable() prints and keeps the requested components", {

  pt <- percTable(tab, col.vars = 2)
  expect_s3_class(pt, "PercTable")
  expect_setequal(setdiff(names(pt), ".printArgs"),
                  c("freq", "perc", "p.row", "p.col"))

  expect_setequal(setdiff(names(percTable(tab, prop = "none")), ".printArgs"),
                  "freq")
  expect_setequal(setdiff(names(percTable(tab, freq = FALSE, prop = "rows")),
                          ".printArgs"),
                  "p.row")
  expect_setequal(setdiff(names(percTable(tab, prop = "none",
                                          expected = TRUE)), ".printArgs"),
                  c("freq", "expected"))

  expect_output(print(pt))
  expect_output(print(percTable(tab, col.vars = 2, margins = 1)))
  expect_output(print(percTable(tab, col.vars = 2, margins = 2)))
  expect_output(print(percTable(tab, col.vars = 2, margins = c(1, 2))))
  expect_output(print(percTable(tab, col.vars = 2,
                                margins = c("rows", "cols"))))
})


test_that("percTable() honours row.vars/col.vars for a single table", {

  # with only one component there is no third dimension; supplying col.vars
  # used to be ignored silently
  a <- capture.output(print(percTable(tab, prop = "none")))
  b <- capture.output(print(percTable(tab, prop = "none", col.vars = 1)))
  expect_false(identical(a, b))
})


test_that("percTable() rejects impossible arguments", {

  expect_error(print(percTable(tab, margins = "total")), "invalid margin")
  expect_error(print(percTable(tab), margins = 3), "must be 1")
  expect_error(percTable(tab, prop = c("none", "rows")), "cannot be combined")
  expect_error(percTable(tab, freq = FALSE, prop = "none"), "nothing to show")
  expect_error(percTable(x = c("a", "b", "a")), "two-dimensional")
})


test_that("percTable() default method splits its dots correctly", {

  # 'exclude' belongs to table(), 'margins' and 'col.vars' to the print method
  x <- factor(c("a", "b", "a", NA), levels = c("a", "b"))
  y <- factor(c("a", "a", "b", "b"), levels = c("a", "b"))

  pt <- percTable(x = x, y = y, col.vars = 2, margins = 1,
                  exclude = character(0), useNA = "ifany")
  expect_s3_class(pt, "PercTable")
  expect_equal(sum(pt$freq), 4)

  pt2 <- percTable(x = x, y = y)
  expect_equal(sum(pt2$freq), 3)
})
