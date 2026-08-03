
test_that("percAgreement() resolves its default input mode", {

  tab <- as.table(matrix(c(20, 5, 3, 12), nrow = 2,
                         dimnames = list(a = c("x", "y"), b = c("x", "y"))))

  # the default used to be reconstructed from formals()$input[[1]], which is
  # the symbol `c`, not "auto"
  r <- percAgreement(tab)
  expect_named(r, c("est", "lci", "uci"))
  expect_equal(unname(r[["est"]]), 32 / 40)

  expect_equal(percAgreement(tab, input = "confusion"), r)
  expect_equal(percAgreement(tab, input = "auto"), r)
})


test_that("percAgreement() gives the same estimate for both input shapes", {

  set.seed(11)
  r1 <- sample(letters[1:3], 40, replace = TRUE)
  r2 <- sample(letters[1:3], 40, replace = TRUE)

  fromRatings <- percAgreement(cbind(r1, r2), input = "ratings")
  fromTable   <- percAgreement(table(r1, r2), input = "confusion")

  expect_equal(unname(fromRatings[["est"]]), unname(fromTable[["est"]]))
  expect_equal(unname(fromRatings[["est"]]), mean(r1 == r2))
})


test_that("percAgreement() reads data frame ratings cell-wise", {

  # as.matrix() on a data frame that is not entirely numeric runs the numeric
  # columns through format(), padding them to a common width WITHIN the
  # column: rater r1 then codes 1 as " 1" while r2 and r3 code it as "1", and
  # the agreement is lost.
  df <- data.frame(r1 = c(1, 10, 2),
                   r2 = c(1, 9, 2),
                   r3 = c("1", "9", "2"),
                   stringsAsFactors = FALSE)

  po <- unname(percAgreement(df, input = "ratings")[["est"]])
  expect_equal(po, (1 + 1 / 3 + 1) / 3)
  expect_false(isTRUE(all.equal(po, 1 / 3)))
})


test_that("percAgreement() skips subjects with fewer than two ratings", {

  x <- rbind(c("a", "a", "a"),
             c("a", "b", NA),
             c("b", NA, NA))

  r <- percAgreement(x, input = "ratings", verbose = TRUE)
  expect_equal(r$nPairable, 2L)
  expect_equal(r$n, 3L)
  expect_equal(r$estimate, (1 + 0) / 2)
})


test_that("percAgreement() confidence bounds stay inside [0, 1]", {

  tab <- as.table(matrix(c(39, 1, 0, 0), nrow = 2,
                         dimnames = list(a = c("x", "y"), b = c("x", "y"))))
  r <- percAgreement(tab)
  expect_true(r[["lci"]] >= 0 && r[["uci"]] <= 1)
  expect_true(r[["lci"]] <= r[["est"]] && r[["est"]] <= r[["uci"]])
})


test_that("percAgreement() validates its arguments", {

  tab <- as.table(matrix(c(20, 5, 3, 12), nrow = 2,
                         dimnames = list(a = c("x", "y"), b = c("x", "y"))))

  expect_error(percAgreement(matrix(1:3, ncol = 1), input = "ratings"),
               "at least two raters")
  expect_error(percAgreement(tab, input = "nonsense"), "should be one of")
  expect_error(percAgreement(tab, conf.level = 1), "conf.level")
  expect_error(percAgreement(tab, fpc = 1), "fpc")
  expect_error(percAgreement(c(1, 2, 3), input = "ratings"),
               "matrix or data frame")
})
