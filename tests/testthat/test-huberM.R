
# ============================================================
# test-huberM.R
# ============================================================

# -----------------------------------------------------------------------
# Point estimate (conf.level = NA)
# -----------------------------------------------------------------------

test_that("point estimate: returns single numeric", {
  expect_true(is.numeric(huberM(1:20)))
  expect_length(huberM(1:20), 1L)
})


test_that("point estimate: close to mean for symmetric normal data", {
  set.seed(1)
  x <- rnorm(500)
  expect_equal(huberM(x), mean(x), tolerance = 0.05)
})


test_that("point estimate: robust against outliers", {
  set.seed(2)
  x_clean   <- rnorm(100)
  x_outlier <- c(x_clean, rep(1000, 5))
  
  expect_lt(abs(huberM(x_outlier)), 1.0)
  expect_gt(abs(mean(x_outlier) - mean(x_clean)), 40)
})


test_that("point estimate: degenerate case (scale = 0) returns initial mu", {
  # All identical values → MAD = 0 → .huberM returns initial mu = median
  x   <- rep(5, 30)
  res <- suppressWarnings(huberM(x))
  expect_equal(res, 5)
})


test_that("point estimate: single-element vector", {
  expect_equal(huberM(42), 42)
})


test_that("point estimate: k parameter changes result", {
  set.seed(3)
  x <- c(rnorm(50), 4, -4)
  
  est_default <- huberM(x, k = 1.345)
  est_tight   <- huberM(x, k = 0.5)
  
  expect_false(isTRUE(all.equal(est_default, est_tight)))
})


# -----------------------------------------------------------------------
# NA handling
# -----------------------------------------------------------------------

test_that("na.rm = FALSE returns NA when NAs present", {
  x <- c(1, 2, NA, 4)
  expect_true(is.na(huberM(x, na.rm = FALSE)))
})


test_that("na.rm = TRUE removes NAs before computing mu and s", {
  # This is the key regression: mu/s must be computed AFTER na.rm
  x    <- c(1, 2, NA, 4, 5)
  xok  <- x[!is.na(x)]
  
  expect_equal(huberM(x, na.rm = TRUE), huberM(xok))
})


test_that("na.rm = TRUE: mu/s not NA even when x has NAs", {
  # If mu = median(x) were evaluated before na.rm, it would be NA
  x   <- c(rnorm(20), NA, NA)
  res <- huberM(x, na.rm = TRUE)
  expect_false(is.na(res))
})


# -----------------------------------------------------------------------
# Wald CI
# -----------------------------------------------------------------------

test_that("wald CI: returns named numeric vector of length 3", {
  set.seed(1)
  x   <- rnorm(50)
  res <- huberM(x, conf.level = 0.95, method = "wald")
  
  expect_true(is.numeric(res))
  expect_length(res, 3L)
  expect_named(res, c("est", "lci", "uci"))
})


test_that("wald CI: est matches point estimate", {
  set.seed(1)
  x   <- rnorm(100)
  res <- huberM(x, conf.level = 0.95, method = "wald")
  
  expect_equal(unname(res["est"]), huberM(x))
})


test_that("wald CI: lci < est < uci", {
  set.seed(4)
  x   <- rnorm(100)
  res <- huberM(x, conf.level = 0.95, method = "wald")
  
  expect_lt(res["lci"], res["est"])
  expect_lt(res["est"], res["uci"])
})


test_that("wald CI: wider at higher conf.level", {
  set.seed(5)
  x    <- rnorm(100)
  ci90 <- huberM(x, conf.level = 0.90, method = "wald")
  ci99 <- huberM(x, conf.level = 0.99, method = "wald")
  
  expect_lt(ci90["uci"] - ci90["lci"], ci99["uci"] - ci99["lci"])
})


test_that("wald CI: narrower with more data", {
  set.seed(6)
  ci_small <- huberM(rnorm(30),  conf.level = 0.95, method = "wald")
  ci_large <- huberM(rnorm(500), conf.level = 0.95, method = "wald")
  
  expect_lt(ci_large["uci"] - ci_large["lci"],
            ci_small["uci"] - ci_small["lci"])
})


test_that("wald CI: symmetric around est for symmetric data", {
  set.seed(7)
  x   <- rnorm(200)
  res <- huberM(x, conf.level = 0.95, method = "wald")
  
  half_lower <- res["est"] - res["lci"]
  half_upper <- res["uci"] - res["est"]
  
  expect_equal(unname(half_lower), unname(half_upper), tolerance = 1e-10)
})


# -----------------------------------------------------------------------
# Bootstrap CI
# -----------------------------------------------------------------------

test_that("boot CI: returns named numeric vector of length 3", {
  set.seed(1)
  x   <- rnorm(50)
  res <- huberM(x, conf.level = 0.95, method = "boot", R = 299)
  
  expect_true(is.numeric(res))
  expect_length(res, 3L)
  expect_named(res, c("est", "lci", "uci"))
})


test_that("boot CI: est matches point estimate", {
  set.seed(1)
  x   <- rnorm(100)
  res <- huberM(x, conf.level = 0.95, method = "boot", R = 299)
  
  expect_equal(unname(res["est"]), huberM(x))
})


test_that("boot CI: lci < est < uci", {
  set.seed(8)
  x   <- rnorm(100)
  res <- huberM(x, conf.level = 0.95, method = "boot", R = 499)
  
  expect_lt(res["lci"], res["est"])
  expect_lt(res["est"], res["uci"])
})


test_that("boot CI: perc and bca give similar results for large n", {
  set.seed(9)
  x    <- rnorm(300)
  perc <- huberM(x, conf.level = 0.95, method = "boot", type = "perc", R = 999)
  bca  <- huberM(x, conf.level = 0.95, method = "boot", type = "bca",  R = 999)
  
  expect_equal(unname(perc["est"]), unname(bca["est"]))
  expect_equal(perc["lci"], bca["lci"], tolerance = 0.1)
  expect_equal(perc["uci"], bca["uci"], tolerance = 0.1)
})


test_that("boot CI: wald and boot agree for large normal sample", {
  set.seed(10)
  x    <- rnorm(500)
  wald <- huberM(x, conf.level = 0.95, method = "wald")
  boot <- huberM(x, conf.level = 0.95, method = "boot", R = 999)
  
  expect_equal(unname(wald["est"]), unname(boot["est"]))
  expect_equal(wald["lci"], boot["lci"], tolerance = 0.05)
  expect_equal(wald["uci"], boot["uci"], tolerance = 0.05)
})


# -----------------------------------------------------------------------
# sides
# -----------------------------------------------------------------------

test_that("sides = 'left' sets uci = Inf", {
  set.seed(1)
  x <- rnorm(50)
  
  res_wald <- huberM(x, conf.level = 0.95, method = "wald", sides = "left")
  expect_equal(unname(res_wald["uci"]), Inf)
  expect_false(is.infinite(res_wald["lci"]))
  
  res_boot <- huberM(x, conf.level = 0.95, method = "boot", sides = "left",
                     R = 299)
  expect_equal(unname(res_boot["uci"]), Inf)
})


test_that("sides = 'right' sets lci = -Inf", {
  set.seed(1)
  x <- rnorm(50)
  
  res_wald <- huberM(x, conf.level = 0.95, method = "wald", sides = "right")
  expect_equal(unname(res_wald["lci"]), -Inf)
  expect_false(is.infinite(res_wald["uci"]))
  
  res_boot <- huberM(x, conf.level = 0.95, method = "boot", sides = "right",
                     R = 299)
  expect_equal(unname(res_boot["lci"]), -Inf)
})


test_that("sides = 'left' wald: lci equals two-sided 90% lci", {
  set.seed(11)
  x        <- rnorm(200)
  left     <- huberM(x, conf.level = 0.95, method = "wald", sides = "left")
  twosided <- huberM(x, conf.level = 0.90, method = "wald", sides = "two.sided")
  
  expect_equal(unname(left["lci"]), unname(twosided["lci"]), tolerance = 1e-10)
})


# -----------------------------------------------------------------------
# Manual mu / s
# -----------------------------------------------------------------------

test_that("manual mu: accepted and passed to engine", {
  set.seed(1)
  x      <- rnorm(50)
  res_default <- huberM(x)
  res_manual  <- huberM(x, mu = mean(x))
  
  # mean ≠ median in general → different result
  expect_false(isTRUE(all.equal(res_default, res_manual)))
})


test_that("manual s = 0: degenerate case handled without error", {
  x <- rnorm(30)
  expect_no_error(suppressWarnings(huberM(x, s = 0)))
})


# -----------------------------------------------------------------------
# Input validation
# -----------------------------------------------------------------------

test_that("non-numeric x raises error", {
  expect_error(huberM(letters[1:5]), "non-empty numeric")
})


test_that("empty vector raises error", {
  expect_error(huberM(numeric(0)), "non-empty numeric")
})


test_that("invalid k raises error", {
  x <- rnorm(20)
  expect_error(huberM(x, k = -1),   "k.*positive")
  expect_error(huberM(x, k = 0),    "k.*positive")
  expect_error(huberM(x, k = Inf),  "k.*finite")
  expect_error(huberM(x, k = "a"),  "k")
})


test_that("non-finite mu raises error", {
  x <- rnorm(20)
  expect_error(huberM(x, mu = NA),  "mu.*finite")
  expect_error(huberM(x, mu = Inf), "mu.*finite")
})


test_that("invalid s raises error", {
  x <- rnorm(20)
  expect_error(huberM(x, s = -1),  "s.*non-negative")
  expect_error(huberM(x, s = Inf), "s.*finite")
})


test_that("invalid conf.level raises error", {
  x <- rnorm(20)
  expect_error(huberM(x, conf.level = 0),   "conf.level")
  expect_error(huberM(x, conf.level = 1.5), "conf.level")
})

