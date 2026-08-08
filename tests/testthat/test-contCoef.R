tab <- apply(HairEyeColor, c(1, 2), sum)


test_that("the point estimate matches the definition", {

  chi <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  n   <- sum(tab)

  expect_equal(contCoef(tab), unname(sqrt(chi / (chi + n))),
               tolerance = 1e-12)

  # the correction is exactly a division by the attainable maximum
  m <- min(dim(tab))
  expect_equal(contCoef(tab, correct = TRUE),
               contCoef(tab) / sqrt((m - 1) / m), tolerance = 1e-12)

  expect_length(contCoef(tab), 1L)
})


test_that("conf.level is validated before it is used", {

  # the function's own default must not be rejected - the type check has
  # to come after NA is allowed, not before it
  expect_silent(contCoef(tab))
  expect_length(contCoef(tab, conf.level = NA), 1L)

  # these used to abort inside if(), with a message about the condition
  expect_error(contCoef(tab, conf.level = c(0.9, 0.95)), "conf.level")
  expect_error(contCoef(tab, conf.level = NULL), "conf.level")

  expect_error(contCoef(tab, conf.level = NaN), "conf.level")
  expect_error(contCoef(tab, conf.level = "0.95"), "conf.level")
  expect_error(contCoef(tab, conf.level = 0), "conf.level")
  expect_error(contCoef(tab, conf.level = 1), "conf.level")
})


test_that("unknown arguments in ... are named, not swallowed", {

  expect_error(contCoef(tab, conf.level = 0.95, Rr = 500), "Rr")
  expect_error(contCoef(tab, conf.level = 0.95, tpye = "bca"), "tpye")

  # boot() arguments that this path cannot honour
  expect_error(contCoef(tab, conf.level = 0.95, ncpus = 2), "ncpus")
  expect_error(contCoef(tab, conf.level = 0.95, parallel = "snow"), "parallel")
})


test_that("bootstrap arguments are validated", {

  expect_error(contCoef(tab, conf.level = 0.95, R = 999.5), "'R'")
  expect_error(contCoef(tab, conf.level = 0.95, R = 1e10), "'R'")
  expect_error(contCoef(tab, conf.level = 0.95, R = -1), "'R'")

  expect_error(contCoef(tab, conf.level = 0.95, type = "norm"), "type")
  expect_error(contCoef(tab, conf.level = 0.95, type = c("perc", "bca")),
               "type")
  
  expect_warning(contCoef(tab, conf.level = 0.95, type = "bca", R = 100),
                 "unstable")
  expect_error(contCoef(tab, conf.level = 0.95, type = "bca", R = 20),
               "at least 49")
})


test_that("R and type never reach the table constructor", {

  set.seed(1)
  a <- contCoef(tab, conf.level = 0.95, R = 300)
  set.seed(1)
  b <- contCoef(tab, conf.level = 0.95, R = 300, type = "perc")

  expect_equal(a, b)
  expect_equal(unname(a["est"]), contCoef(tab), tolerance = 1e-12)
})


test_that("the interval stays inside the attainable range", {

  m    <- min(dim(tab))
  cMax <- sqrt((m - 1) / m)

  set.seed(2)
  ci <- contCoef(tab, conf.level = 0.95, R = 400)

  expect_named(ci, c("est", "lci", "uci"))
  expect_true(ci[["lci"]] >= 0 && ci[["uci"]] <= cMax)
  expect_true(ci[["lci"]] <= ci[["est"]] && ci[["est"]] <= ci[["uci"]])

  set.seed(2)
  cis <- contCoef(tab, conf.level = 0.95, R = 400, correct = TRUE)
  expect_true(cis[["uci"]] <= 1)
})


test_that("sides names the side carrying the finite bound", {

  m    <- min(dim(tab))
  cMax <- sqrt((m - 1) / m)

  set.seed(3); two   <- contCoef(tab, conf.level = 0.95, R = 400)
  set.seed(3); left  <- contCoef(tab, conf.level = 0.95, R = 400, sides = "left")
  set.seed(3); right <- contCoef(tab, conf.level = 0.95, R = 400, sides = "right")

  # the estimate does not depend on the sidedness
  expect_equal(two[["est"]], left[["est"]])
  expect_equal(two[["est"]], right[["est"]])

  # "left" is the analogue of alternative = "greater": finite lower bound
  expect_equal(left[["uci"]], cMax)
  expect_equal(right[["lci"]], 0)

  # a one-sided bound carries the whole alpha, so it is tighter
  expect_true(left[["lci"]]  >= two[["lci"]])
  expect_true(right[["uci"]] <= two[["uci"]])
})


test_that("a one-sided interval below conf.level 0.5 is not turned around", {
  
  m <- min(dim(tab)); cMax <- sqrt((m - 1) / m)
  
  set.seed(4)
  ci <- contCoef(tab, conf.level = 0.4, R = 400, sides = "left")
  
  # unter 50 % liegt die Untergrenze ÜBER dem Schätzer - das ist korrekt
  expect_true(is.finite(ci[["lci"]]))
  expect_true(ci[["lci"]] <= ci[["uci"]])
  expect_equal(ci[["uci"]], cMax)
  
  set.seed(4)
  wide <- contCoef(tab, conf.level = 0.9, R = 400, sides = "left")
  expect_true(ci[["lci"]] >= wide[["lci"]])
  
  # die Seite selbst: left(c) liest dasselbe Quantil wie two.sided(2c-1)
  set.seed(7); l <- contCoef(tab, conf.level = 0.95, R = 400, sides = "left")
  set.seed(7); t <- contCoef(tab, conf.level = 0.90, R = 400)
  expect_equal(l[["lci"]], t[["lci"]])
})




test_that("the bootstrap follows set.seed", {

  set.seed(5); a <- contCoef(tab, conf.level = 0.95, R = 300)
  set.seed(5); b <- contCoef(tab, conf.level = 0.95, R = 300)
  expect_identical(a, b)

  set.seed(6); c1 <- contCoef(tab, conf.level = 0.95, R = 300)
  expect_false(isTRUE(all.equal(a[["lci"]], c1[["lci"]])))
})


test_that("bca and perc agree on the estimate and both stay in range", {

  m    <- min(dim(tab))
  cMax <- sqrt((m - 1) / m)

  set.seed(8); p <- contCoef(tab, conf.level = 0.95, R = 999, type = "perc")
  set.seed(8); b <- contCoef(tab, conf.level = 0.95, R = 999, type = "bca")

  # the estimate comes from the table, never from the interval machinery
  expect_equal(p[["est"]], b[["est"]])

  expect_true(b[["lci"]] >= 0 && b[["uci"]] <= cMax)
  expect_true(b[["lci"]] <= b[["est"]] && b[["est"]] <= b[["uci"]])

  # both read the same replicates, so they should be close but not equal
  expect_equal(b[["lci"]], p[["lci"]], tolerance = 0.05)
})


test_that("the compiled pieces agree with a plain R jackknife", {

  small <- matrix(c(12, 5, 3, 9), nrow = 2)

  cells <- which(small >= 1, arr.ind = TRUE)
  jack  <- apply(cells, 1L, function(ij) {
    m <- small
    m[ij[1], ij[2]] <- m[ij[1], ij[2]] - 1
    contCoef(m)
  })
  cnt <- small[small >= 1]

  meanJ <- sum(cnt * jack) / sum(small)
  d     <- meanJ - jack
  aRef  <- sum(cnt * d^3) / (6 * sum(cnt * d^2)^1.5)

  expect_equal(contcoef_jackknife_a_cpp(small, FALSE), aRef,
               tolerance = 1e-10)
})


test_that("a table without observations does not reach the bootstrap", {

  empty <- matrix(0L, 2, 2)

  expect_true(is.na(contCoef(empty)))

  ci <- contCoef(empty, conf.level = 0.95)
  expect_named(ci, c("est", "lci", "uci"))
  expect_true(all(is.na(ci)))
})
