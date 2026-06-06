# Tests migriert von alter ccc-API (Liste) auf neue API (named numeric vector
# mit Attributen). Weggefallen: $blalt (Bland-Altman-Frame), $rho.c als
# data.frame. Neu: attr(res, "biasCorrection") statt $C.b.

# ccc() — Rückgabestruktur ------------------------------------------------

test_that("ccc ohne conf.level gibt named numeric vector mit 'est' zurück", {
  x <- 1:20
  y <- 1:20
  res <- ccc(x, y)
  expect_type(res, "double")
  expect_named(res, "est")
})

test_that("ccc mit conf.level gibt 'est', 'lci', 'uci' zurück", {
  x <- rnorm(40, 5, 2)
  y <- x + rnorm(40, 0, 0.5)
  res <- ccc(x, y, conf.level = 0.95)
  expect_named(res, c("est", "lci", "uci"))
})

# ccc() — Punktschätzer ---------------------------------------------------

test_that("ccc rhoC ist nahe 1 bei perfekter Übereinstimmung", {
  x <- 1:50
  y <- 1:50
  res <- ccc(x, y)
  expect_gt(res[["est"]], 0.999)
})

test_that("ccc rhoC ist nahe 0 für unkorrelierte Variablen", {
  set.seed(7)
  x <- rnorm(200)
  y <- rnorm(200)
  res <- ccc(x, y)
  expect_lt(abs(res[["est"]]), 0.2)
})

# ccc() — Konfidenzintervall ----------------------------------------------

test_that("ccc CI: lci < est < uci", {
  set.seed(1)
  x <- rnorm(60, 5, 2)
  y <- x + rnorm(60, 0, 1)
  res <- ccc(x, y, conf.level = 0.95)
  expect_lt(res[["lci"]], res[["est"]])
  expect_gt(res[["uci"]], res[["est"]])
})

test_that("ccc method = 'asymptotic' gibt gültigen Schätzer zurück", {
  set.seed(2)
  x <- rnorm(50, 5, 2)
  y <- x + rnorm(50, 0, 1)
  res <- ccc(x, y, conf.level = 0.95, method = "asymptotic")
  expect_gte(res[["est"]], -1)
  expect_lte(res[["est"]],  1)
})

# ccc() — Attribute -------------------------------------------------------

test_that("attr biasCorrection ist 1 bei perfekter Übereinstimmung (x == y)", {
  x <- 1:30
  res <- ccc(x, x)
  expect_equal(attr(res, "biasCorrection"), 1, tolerance = 1e-6)
})

test_that("attr nObs stimmt mit Eingabelänge überein", {
  x <- rnorm(35)
  y <- x + rnorm(35, sd = 0.3)
  res <- ccc(x, y)
  expect_equal(attr(res, "nObs"), 35L)
})

# ccc() — NA-Behandlung ---------------------------------------------------

test_that("ccc na.rm = TRUE entfernt NA-Paare vor Berechnung", {
  x_na    <- c(1:10, NA)
  y_na    <- c(1:10, 5)
  res_narm  <- ccc(x_na, y_na, na.rm = TRUE)
  res_clean <- ccc(1:10, 1:10)
  expect_equal(res_narm[["est"]], res_clean[["est"]], tolerance = 1e-6)
})

test_that("ccc na.rm = FALSE mit NA gibt NA zurück", {
  expect_true(is.na(ccc(c(1, NA, 3), c(1, 2, 3))))
})

# .cccPoint ---------------------------------------------------------------

test_that(".cccPoint: perfect agreement returns 1", {
  x <- 1:5
  expect_equal(.cccPoint(x, x), 1)
})

test_that(".cccPoint: perfect inversion returns -1", {
  x <- c(-2, -1, 0, 1, 2)
  expect_equal(.cccPoint(x, -x), -1)
})

test_that(".cccPoint: scale shift reduces rhoC below 1", {
  x <- 1:5
  r <- .cccPoint(x, 2 * x)
  expect_gt(r, 0)
  expect_lt(r, 1)
})

test_that(".cccPoint: location shift reduces rhoC below 1", {
  x <- 1:5
  r <- .cccPoint(x, x + 10)
  expect_gt(r, 0)
  expect_lt(r, 1)
})

test_that(".cccPoint: output is in [-1, 1]", {
  set.seed(1)
  x <- rnorm(50)
  y <- x + rnorm(50, sd = 0.5)
  r <- .cccPoint(x, y)
  expect_gte(r, -1)
  expect_lte(r,  1)
})

test_that(".cccPoint: symmetric in x and y", {
  x <- c(2, 4, 6, 8, 10)
  y <- c(1, 3, 5, 7, 9)
  expect_equal(.cccPoint(x, y), .cccPoint(y, x))
})

test_that(".cccPoint: zero correlation yields rhoC == 0", {
  x <- c( 1, -1,  1, -1)
  y <- c( 1,  1, -1, -1)
  expect_equal(.cccPoint(x, y), 0)
})

test_that(".cccPoint: works with n = 3", {
  expect_no_error(.cccPoint(1:3, 1:3))
})

test_that(".cccPoint: stable for large n", {
  set.seed(42)
  n <- 10000
  x <- seq(0, 1, length.out = n)
  y <- x + rnorm(n, sd = 0.01)
  expect_gt(.cccPoint(x, y), 0.99)
})

# .makeEstimateResult -----------------------------------------------------

test_that(".makeEstimateResult: est only returns length-1 named vector", {
  r <- .makeEstimateResult(est = 0.9)
  expect_named(r, "est")
  expect_equal(unname(r), 0.9)
})

test_that(".makeEstimateResult: lci/uci appended when provided", {
  r <- .makeEstimateResult(est = 0.9, lci = 0.8, uci = 0.95)
  expect_named(r, c("est", "lci", "uci"))
})

test_that(".makeEstimateResult: NULL lci/uci not added", {
  r <- .makeEstimateResult(est = 0.5)
  expect_false("lci" %in% names(r))
  expect_false("uci" %in% names(r))
})

test_that(".makeEstimateResult: attrs stored as attributes", {
  r <- .makeEstimateResult(est = 0.5, attrs = list(nObs = 42L))
  expect_equal(attr(r, "nObs"), 42L)
})

test_that(".makeEstimateResult: empty attrs add nothing", {
  r <- .makeEstimateResult(est = 0.5, attrs = list())
  expect_equal(names(attributes(r)), "names")
})

# integration -------------------------------------------------------------

test_that(".cccPoint result stored correctly via makeEstimateResult", {
  x <- 1:10
  y <- 1:10
  est <- .cccPoint(x, y)
  r <- .makeEstimateResult(est = est, attrs = list(nObs = 10L))
  expect_equal(r[["est"]], 1)
  expect_equal(attr(r, "nObs"), 10L)
})

