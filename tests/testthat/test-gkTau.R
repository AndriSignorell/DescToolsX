tab <- as.table(rbind(c(26, 26, 23, 18, 9),
                      c(6, 7, 9, 14, 23)))

tt <- matrix(c(549, 93, 233, 119, 225, 455, 402,
               212, 124, 78, 42, 41, 12, 132,
               54, 54, 33, 13, 46, 7, 153), ncol = 3)


test_that("gkTau returns a bare numeric without conf.level", {

  res <- gkTau(tab)
  expect_length(res, 1L)
  expect_true(is.numeric(res))
  expect_null(names(res))
  expect_gte(res, 0)
  expect_lte(res, 1)
})


test_that("gkTau reproduces the reference values", {

  # values recomputed independently from the Liebetrau (1983) formulas
  expect_equal(unname(gkTau(tab, direction = "column")), 0.0412165798,
               tolerance = 1e-8)
  expect_equal(unname(gkTau(tab, direction = "row")), 0.1652331516,
               tolerance = 1e-8)

  expect_equal(gkTau(tab, direction = "row", conf.level = 0.95),
               c(est = 0.1652331516, lci = 0.0492148420, uci = 0.2812514611),
               tolerance = 1e-7)
  expect_equal(gkTau(tt, direction = "column", conf.level = 0.95),
               c(est = 0.0860191965, lci = 0.0724394269, uci = 0.0995989660),
               tolerance = 1e-7)
})


test_that("gkTau reduces to phi^2 in the 2x2 case", {

  t2 <- as.table(cbind(c(11, 2), c(4, 6)))

  expect_equal(unname(gkTau(t2, direction = "row")), unname(phi(t2)^2),
               tolerance = 1e-8)
  expect_equal(unname(gkTau(t2, direction = "column")), unname(phi(t2)^2),
               tolerance = 1e-8)

  # same claim once more without relying on phi(): chi^2 / n
  phi2 <- unname(chisq.test(t2, correct = FALSE)$statistic / sum(t2))
  expect_equal(unname(gkTau(t2, direction = "row")), phi2, tolerance = 1e-8)
  expect_equal(unname(gkTau(t2, direction = "column")), phi2, tolerance = 1e-8)
})


test_that("direction is a transposition, not a separate formula", {

  tauRow <- gkTau(tab, direction = "row")
  tauCol <- gkTau(tab, direction = "column")

  # the measure is asymmetric, the two directions must not coincide
  expect_false(isTRUE(all.equal(tauRow, tauCol)))
  expect_equal(unname(gkTau(t(tab), direction = "row")), unname(tauCol))

  # partial matching of the value still works
  expect_equal(gkTau(tab, direction = "col"), tauCol)
  expect_error(gkTau(tab, direction = "diagonal"))
})


test_that("empty rows and columns do not change the result", {

  padded <- rbind(cbind(tab, 0), 0)
  expect_equal(unname(gkTau(padded, direction = "row")),
               unname(gkTau(tab, direction = "row")))
})


test_that("the vector interface passes its dots to table()", {

  x <- c(1, 2, 2, 3)
  y <- c(1, 1, 2, 3)
  expect_length(gkTau(x, y), 1L)

  set.seed(11)
  x <- sample(letters[1:3], 200, replace = TRUE)
  y <- sample(letters[1:4], 200, replace = TRUE)

  expect_equal(gkTau(x, y), gkTau(table(x, y)))

  x[1:10] <- NA
  expect_equal(gkTau(x, y, useNA = "ifany"),
               gkTau(table(x, y, useNA = "ifany")))
  # dots without y have nowhere to go and must not vanish silently
  expect_error(gkTau(tab, useNA = "ifany"), "table")
})


test_that("the confidence interval brackets the estimate", {

  res <- gkTau(tab, direction = "row", conf.level = 0.95)

  expect_length(res, 3L)
  expect_named(res, c("est", "lci", "uci"))
  expect_lte(res[["lci"]], res[["est"]])
  expect_gte(res[["uci"]], res[["est"]])
  # clamped to the range of tau
  expect_gte(res[["lci"]], 0)
  expect_lte(res[["uci"]], 1)
})


test_that("one-sided intervals close at the range bound", {

  two <- gkTau(tab, direction = "row", conf.level = 0.95)
  left <- gkTau(tab, direction = "row", conf.level = 0.975, sides = "left")
  right <- gkTau(tab, direction = "row", conf.level = 0.975, sides = "right")

  # the two one-sided bounds at 0.975 are the ends of the two-sided 0.95
  # interval, and the open side sits at 0 or 1 rather than at an infinity
  expect_equal(sort(c(left[["lci"]], left[["uci"]],
                      right[["lci"]], right[["uci"]])),
               sort(c(0, two[["lci"]], two[["uci"]], 1)))

  expect_error(gkTau(tab, conf.level = 0.5, sides = "left"), "one-sided")
  expect_error(gkTau(tab, conf.level = 0.4, sides = "right"), "one-sided")
})


test_that("conf.level is validated and NA gives a bare estimate", {

  expect_error(gkTau(tab, conf.level = c(0.9, 0.95)))
  expect_error(gkTau(tab, conf.level = NULL))
  expect_error(gkTau(tab, conf.level = NaN))
  expect_error(gkTau(tab, conf.level = 1.2))
})


test_that("a vanishing standard error gives NA bounds, not a point interval", {

  # perfect prediction: sigma2 is exactly 0
  perfect <- as.table(matrix(c(5, 0, 0, 5), nrow = 2))
  expect_warning(res <- gkTau(perfect, conf.level = 0.95), "standard error")
  expect_equal(res[["est"]], 1)
  expect_true(all(is.na(res[c("lci", "uci")])))
  # the point estimate alone must stay silent
  expect_silent(gkTau(perfect))

  # a single non-empty row: tau is 0 and the variance vanishes as well
  one <- as.table(matrix(c(3, 4, 5, 0, 0, 0), nrow = 2, byrow = TRUE))
  expect_warning(res <- gkTau(one, direction = "column", conf.level = 0.95))
  expect_equal(res[["est"]], 0)
  expect_true(all(is.na(res[c("lci", "uci")])))
})


test_that("independent tables give exactly zero", {

  flat <- as.table(matrix(rep(25, 4), nrow = 2))
  expect_identical(unname(gkTau(flat)), 0)

  expect_identical(unname(gkTau(outer(c(10, 20, 30), c(1, 2, 3)),
                                direction = "row")), 0)

  # exact independence is only exact in theory: this table evaluates to
  # +-2 * .Machine$double.eps and used to slip past the check
  indep <- outer(c(850, 783), c(198, 71))
  expect_identical(unname(gkTau(indep, direction = "row")), 0)
  expect_identical(unname(gkTau(indep, direction = "column")), 0)
  expect_warning(res <- gkTau(indep, conf.level = 0.95), "standard error")
  expect_true(all(is.na(res[c("lci", "uci")])))
})


test_that("degenerate and malformed input is rejected", {

  # dependent variable with a single category: 0/0
  expect_error(gkTau(as.table(matrix(c(3, 4, 5), ncol = 1)),
                     direction = "column"), "not defined")
  expect_error(gkTau(as.table(matrix(c(3, 4, 5), nrow = 1)),
                     direction = "row"), "not defined")

  # is.table() is TRUE for 3d arrays as well
  expect_error(gkTau(as.table(array(1:8, dim = c(2, 2, 2)))),
               "two-dimensional")
  expect_error(gkTau(1:10), "two-dimensional")

  expect_error(gkTau(matrix(c(1, 2, NA, 4), nrow = 2)), "NA")
  expect_error(gkTau(matrix(c(1, 2, Inf, 4), nrow = 2)), "finite")
  expect_error(gkTau(matrix(c(1, 2, NaN, 4), nrow = 2)))
  expect_error(gkTau(matrix(c(1, 2, -3, 4), nrow = 2)), "negative")
  expect_error(gkTau(matrix(letters[1:4], nrow = 2)), "numeric")
})


test_that("the argument order holds", {

  # signature is (x, y, conf.level, sides, direction, ...)
  expect_equal(gkTau(tab, NULL, 0.95, "two.sided", "column"),
               gkTau(tab, conf.level = 0.95, direction = "column"))
})
