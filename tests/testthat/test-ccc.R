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



# Review 25.09.2026: interval methods, sides, degenerate cases, validation ----

.cccRef <- function(x, y) {
  # Lin (1989/2000) in the textbook form, independent of the rewritten
  # expression inside .cccEngine()
  n   <- length(x)
  sx2 <- var(x) * (n - 1) / n
  sy2 <- var(y) * (n - 1) / n
  r   <- cor(x, y)
  rho <- 2 * cov(x, y) * (n - 1) / n / (sx2 + sy2 + (mean(y) - mean(x))^2)
  u   <- (mean(y) - mean(x)) / (sx2 * sy2)^0.25
  v   <- ((1 - r^2) * rho^2 * (1 - rho^2) / r^2 +
            2 * rho^3 * (1 - rho) * u^2 / r -
            rho^4 * u^4 / (2 * r^2)) / (n - 2)
  list(rho = rho, se = sqrt(v))
}

set.seed(11)
xr <- rnorm(40, 10, 2)
yr <- xr + rnorm(40, 0.5, 1)

test_that("asymptotic and z-transform intervals follow Lin's variance", {
  ref <- .cccRef(xr, yr)
  q   <- qnorm(0.975)

  a <- ccc(xr, yr, conf.level = 0.95, method = "asymptotic")
  expect_equal(unname(a[["est"]]), ref$rho)
  expect_equal(unname(a[c("lci", "uci")]), ref$rho + c(-1, 1) * q * ref$se)

  z <- ccc(xr, yr, conf.level = 0.95)
  seZ <- ref$se / (1 - ref$rho^2)
  expect_equal(unname(z[c("lci", "uci")]),
               tanh(atanh(ref$rho) + c(-1, 1) * q * seZ))
  expect_identical(attr(z, "method"), "z-transform")
})

test_that("one-sided intervals put the open side at the range boundary", {
  ref <- .cccRef(xr, yr)
  q   <- qnorm(0.95)
  for (m in c("z-transform", "asymptotic")) {
    l <- ccc(xr, yr, conf.level = 0.95, sides = "left", method = m)
    r <- ccc(xr, yr, conf.level = 0.95, sides = "right", method = m)
    expect_equal(unname(l[["uci"]]), 1, info = m)
    expect_equal(unname(r[["lci"]]), -1, info = m)
    # the finite bound equals the corresponding 90% two-sided bound
    two <- ccc(xr, yr, conf.level = 0.90, method = m)
    expect_equal(unname(l[["lci"]]), unname(two[["lci"]]), info = m)
    expect_equal(unname(r[["uci"]]), unname(two[["uci"]]), info = m)
  }
  expect_error(ccc(xr, yr, conf.level = 0.4, sides = "left"), "exceed 0.5")
})

test_that("the bootstrap interval contains the estimate and honours sides", {
  set.seed(3)
  b <- ccc(xr, yr, conf.level = 0.95, method = "boot", R = 999)
  expect_named(b, c("est", "lci", "uci"))
  expect_true(b[["lci"]] <= b[["est"]] && b[["est"]] <= b[["uci"]])
  set.seed(3)
  expect_equal(ccc(xr, yr, conf.level = 0.95, method = "boot", R = 999), b)
  set.seed(4)
  expect_equal(unname(ccc(xr, yr, conf.level = 0.95, method = "boot",
                          R = 999, sides = "left")[["uci"]]), 1)
  set.seed(4)
  expect_equal(unname(ccc(xr, yr, conf.level = 0.95, method = "boot",
                          R = 999, sides = "right")[["lci"]]), -1)
})

test_that("perfect concordance gives NA bounds with a warning, not (1, 1)", {
  for (m in c("z-transform", "asymptotic")) {
    expect_warning(res <- ccc(1:10, 1:10, conf.level = 0.95, method = m),
                   "variance of the CCC is zero")
    expect_equal(unname(res[["est"]]), 1)
    expect_true(is.na(res[["lci"]]) && is.na(res[["uci"]]))
  }
})

test_that("missing values without na.rm give an NA result of full shape", {
  res <- ccc(c(1, NA, 3, 4), c(1, 2, 3, 5), conf.level = 0.95)
  expect_named(res, c("est", "lci", "uci"))
  expect_true(all(is.na(res)))
  expect_identical(attr(res, "nObs"), 4L)
})

test_that("ccc() validates its input", {
  expect_error(ccc(letters[1:3], 1:3), "'x' must be a numeric vector")
  expect_error(ccc(1:3, matrix(1:3)), "'y' must be a numeric vector")
  expect_error(ccc(1:3, 1:4), "equal length")
  expect_error(ccc(1:3, 1:3, na.rm = NA), "na.rm")
  for (cl in list("0.9", c(0.9, 0.95), NaN, 0, 1, 1.5))
    expect_error(ccc(1:5, c(2, 1, 4, 3, 5), conf.level = cl), "conf.level")
  expect_error(ccc(1:2, 1:2), "at least 3")
  expect_error(ccc(c(1, 2, NA), c(1, NA, 3), na.rm = TRUE), "fewer than 3")
  expect_error(ccc(c(1, 2, Inf), 1:3), "infinite")
  expect_error(ccc(1:3, c(1, 2, Inf)), "infinite")
  expect_error(ccc(rep(1, 5), 1:5), "positive variance")
  expect_error(ccc(1:5, rep(1, 5)), "positive variance")
})

test_that(".cccPoint is NA for two identical constant vectors", {
  expect_true(is.na(.cccPoint(rep(2, 4), rep(2, 4))))
})


test_that("NA bounds survive applySides(); the open side is still reported", {
  expect_warning(res <- ccc(1:10, 1:10, conf.level = 0.95, sides = "left"),
                 "variance of the CCC is zero")
  expect_true(is.na(res[["lci"]]))
  expect_equal(unname(res[["uci"]]), 1)
})
