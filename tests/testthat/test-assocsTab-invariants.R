
# Invariants that hold for any correct implementation of the ordinal
# association measures - they need no reference values and would have
# caught both variance bugs found in the review.

test_that("Somers' D is symmetric under transposition + direction swap", {

  tab <- as.table(matrix(c(12,  3,  1,
                            5, 14,  6,
                            2,  7, 18), nrow = 3, byrow = TRUE))

  a <- ordAssocs(tab,   conf.level = 0.95, direction = "row",    which = "somers")$somers
  b <- ordAssocs(t(tab), conf.level = 0.95, direction = "column", which = "somers")$somers

  # D(R|C) on tab is D(C|R) on t(tab) - estimate AND interval
  expect_equal(unname(a), unname(b), tolerance = 1e-10)
})


test_that("the c-statistic interval stays inside [0, 1]", {

  tab <- as.table(matrix(c(20, 1, 1, 20), nrow = 2))
  cs <- ordAssocs(tab, conf.level = 0.99, which = "cstat")$cstat

  expect_gte(unname(cs["lci"]), 0)
  expect_lte(unname(cs["uci"]), 1)
})


test_that("variances vanish for a perfectly concordant table", {

  # a strictly diagonal table has gamma = tau = somers = 1 with no
  # sampling variation left, so every interval must collapse onto the
  # estimate. An uncentred second moment gives ~4/n instead of 0 for
  # tau-a and tau-c.
  tab <- as.table(diag(c(30, 30, 30)))

  res <- ordAssocs(tab, conf.level = 0.95)

  for (nm in c("gamma", "tauA", "tauC", "somers")) {
    expect_equal(unname(res[[nm]]["lci"]), unname(res[[nm]]["est"]),
                 tolerance = 1e-8, label = nm)
    expect_equal(unname(res[[nm]]["uci"]), unname(res[[nm]]["est"]),
                 tolerance = 1e-8, label = nm)
  }
})


test_that("intervals cover the estimate and widen with conf.level", {

  tab <- as.table(matrix(c(15,  8,  4,
                            9, 17, 10,
                            3, 11, 21), nrow = 3, byrow = TRUE))

  narrow <- ordAssocs(tab, conf.level = 0.90)
  wide   <- ordAssocs(tab, conf.level = 0.99)

  for (nm in names(narrow)) {
    expect_lte(unname(narrow[[nm]]["lci"]), unname(narrow[[nm]]["est"]))
    expect_gte(unname(narrow[[nm]]["uci"]), unname(narrow[[nm]]["est"]))

    expect_lte(unname(wide[[nm]]["lci"]), unname(narrow[[nm]]["lci"]) + 1e-12)
    expect_gte(unname(wide[[nm]]["uci"]), unname(narrow[[nm]]["uci"]) - 1e-12)
  }
})


test_that("direction is refused rather than ignored in vector mode", {
  expect_error(
    ordAssocs(swiss$Fertility, swiss$Agriculture, direction = "column"),
    "table mode"
  )
})


test_that("as.ym survives NA and out-of-range input", {
  expect_equal(unclass(as.ym(c(201201, NA, 201513, 999901))),
               c(201201L, NA, NA, NA))
})


test_that("ym arithmetic keeps its class and can be chained", {
  expect_s3_class(as.ym(201511) + 5, "ym")
  expect_equal(unclass(as.ym(201511) + 5 - 2), 201602L)
  expect_equal(unclass(addMonths(as.ym(c(201511, 201302)), c(5, -4))),
               c(201604L, 201210L))
})


test_that("abstract() handles a data frame without rows or columns", {

  noCols <- abstract(data.frame())
  expect_equal(nrow(noCols), 0L)
  expect_equal(colnames(noCols),
               c("Nr", "Class", "ColName", "NAs", "Levels", "Label"))

  noRows <- abstract(data.frame(a = integer(0)))
  expect_equal(nrow(noRows), 1L)
})
