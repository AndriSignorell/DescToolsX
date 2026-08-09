# a confusion matrix needs matching dimnames - isConfusionTable() reads a
# bare matrix as a ratings matrix, which is what input = "auto" then does
cm <- as.table(matrix(c(40,  5,
                         8, 47), nrow = 2,
                      dimnames = list(pred = c("a", "b"),
                                      obs  = c("a", "b"))))

set.seed(1)
rat <- matrix(sample(1:3, 60, replace = TRUE), ncol = 3)


test_that("percAgreement returns a bare estimate by default", {

  # The default moved from conf.level = 0.95 to NA, in line with the rest
  # of the family (ccc, cohenKappa, pabak). That change is SILENT for
  # anyone reading res[["est"]] - it starts returning NA instead of
  # failing - so it gets pinned here.
  expect_length(percAgreement(cm), 1L)
  expect_null(names(percAgreement(cm)))
  expect_identical(formals(percAgreement)$conf.level, NA)

  expect_equal(unname(percAgreement(cm)), sum(diag(cm)) / sum(cm))
})


test_that("the arguments follow the current naming and order", {

  a <- names(formals(percAgreement))

  expect_false("verbose" %in% a)
  expect_true("output" %in% a)
  expect_true("sides" %in% a)

  # everything before conf.level is a data argument (design_rules 4.1)
  expect_equal(a[seq_len(match("conf.level", a) - 1L)], c("x", "y"))
  expect_equal(a[match("conf.level", a) + 1L], "sides")
})


test_that("conf.level is validated through the shared helper", {

  expect_error(percAgreement(cm, conf.level = c(0.9, 0.95)), "conf.level")
  expect_error(percAgreement(cm, conf.level = NULL), "conf.level")
  expect_error(percAgreement(cm, conf.level = NaN), "conf.level")
  expect_error(percAgreement(cm, conf.level = 0), "conf.level")
  expect_error(percAgreement(cm, conf.level = 1), "conf.level")

  expect_error(percAgreement(cm, fpc = 1), "fpc")
  expect_error(percAgreement(cm, fpc = -0.1), "fpc")
})


test_that("sides closes the open side at 0 and 1", {

  two   <- percAgreement(cm, conf.level = 0.95)
  left  <- percAgreement(cm, conf.level = 0.95, sides = "left")
  right <- percAgreement(cm, conf.level = 0.95, sides = "right")

  expect_named(two, c("est", "lci", "uci"))

  expect_equal(left[["uci"]], 1)
  expect_equal(right[["lci"]], 0)

  expect_equal(left[["est"]], two[["est"]])
  expect_gte(left[["lci"]],  two[["lci"]])
  expect_lte(right[["uci"]], two[["uci"]])

  # left(gamma) reads the same end as two.sided(2*gamma - 1)
  expect_equal(left[["lci"]], percAgreement(cm, conf.level = 0.90)[["lci"]])
  expect_equal(right[["uci"]], percAgreement(cm, conf.level = 0.90)[["uci"]])

  expect_true(two[["lci"]] >= 0 && two[["uci"]] <= 1)
})


test_that("a one-sided interval below conf.level 0.5 is refused", {

  expect_error(percAgreement(cm, conf.level = 0.4, sides = "left"), "0.5")
  expect_error(percAgreement(cm, conf.level = 0.5, sides = "right"), "0.5")
  expect_silent(percAgreement(cm, conf.level = 0.4))

  # matched even when no interval is requested
  expect_error(percAgreement(cm, sides = "links"), "two.sided")
})


test_that("unknown arguments are rejected rather than swallowed", {

  # '...' used to be documented as reserved for future extensions, which
  # meant a misspelled name disappeared without a word
  expect_error(percAgreement(cm, verbose = TRUE), "verbose")
  expect_error(percAgreement(cm, conf.levl = 0.95), "conf.levl")
})


test_that("output = 'ext' carries the same interval as output = 'def'", {

  d <- percAgreement(cm, conf.level = 0.95)
  e <- percAgreement(cm, conf.level = 0.95, output = "ext")

  expect_equal(e$ci, d)
  expect_equal(e$est, unname(d[["est"]]))
  expect_true(is.finite(e$se))
  expect_equal(e$n, sum(cm))

  # sides applies in the extended output too
  g <- percAgreement(cm, conf.level = 0.95, output = "ext", sides = "left")
  expect_equal(g$ci[["uci"]], 1)

  # and without an interval the triple is still indexable
  f <- percAgreement(cm, output = "ext")
  expect_named(f$ci, c("est", "lci", "uci"))
  expect_true(is.na(f$ci[["lci"]]))
})


test_that("the ratings interface still works and agrees with itself", {

  r <- percAgreement(rat, conf.level = 0.95)

  expect_named(r, c("est", "lci", "uci"))
  expect_true(r[["est"]] >= 0 && r[["est"]] <= 1)
  expect_true(r[["lci"]] <= r[["est"]] && r[["est"]] <= r[["uci"]])

  # input = "auto" resolves to the same thing as naming it
  expect_equal(percAgreement(rat, conf.level = 0.95, input = "ratings"), r)
  expect_equal(percAgreement(cm, conf.level = 0.95, input = "confusion"),
               percAgreement(cm, conf.level = 0.95))
})


test_that("too little data gives NA bounds rather than a made-up interval", {

  one <- as.table(matrix(c(1, 0, 0, 0), nrow = 2,
                         dimnames = list(c("a", "b"), c("a", "b"))))
  res <- suppressWarnings(percAgreement(one, conf.level = 0.95))

  expect_true(is.na(res[["lci"]]))
  expect_true(is.na(res[["uci"]]))
})
