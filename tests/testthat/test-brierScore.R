

# ============================================================
# test-brierScore.R
# ============================================================

# shared test data
local_data <- local({
  set.seed(42)
  resp <- rbinom(200, 1, 0.4)
  pred <- plogis(rnorm(200, ifelse(resp == 1, 0.5, -0.5)))
  list(resp = resp, pred = pred)
})

resp <- local_data$resp
pred <- local_data$pred


# -----------------------------------------------------------------------
# Point estimate
# -----------------------------------------------------------------------

test_that("point estimate: returns single numeric", {
  res <- brierScore(resp, pred)
  expect_true(is.numeric(res))
  expect_length(res, 1L)
})


test_that("point estimate: correct manual calculation", {
  loss     <- resp * (1 - pred)^2 + (1 - resp) * pred^2
  expected <- mean(loss)
  expect_equal(brierScore(resp, pred), expected)
})


test_that("point estimate: perfect predictions score 0", {
  r <- c(0L, 0L, 1L, 1L)
  p <- c(0,   0,  1,  1 )
  expect_equal(brierScore(r, p), 0)
})


test_that("point estimate: random predictions score ~0.25", {
  set.seed(1)
  r <- rbinom(10000, 1, 0.5)
  p <- rep(0.5, 10000)
  expect_equal(brierScore(r, p), 0.25, tolerance = 0.01)
})


test_that("point estimate: scaled score is 1 for perfect model", {
  r <- c(0L, 0L, 1L, 1L)
  p <- c(0,   0,  1,  1 )
  expect_equal(brierScore(r, p, scaled = TRUE), 1)
})


test_that("point estimate: scaled score is 0 for climatological baseline", {
  set.seed(1)
  r    <- rbinom(1000, 1, 0.4)
  p    <- rep(mean(r), 1000)
  res  <- brierScore(r, p, scaled = TRUE)
  expect_equal(res, 0, tolerance = 1e-10)
})


test_that("point estimate: scaled score in (-Inf, 1] in general", {
  res <- brierScore(resp, pred, scaled = TRUE)
  expect_lte(res, 1)
})


# -----------------------------------------------------------------------
# Normal CI
# -----------------------------------------------------------------------

test_that("normal CI: returns named numeric vector of length 3", {
  res <- brierScore(resp, pred, conf.level = 0.95, method = "normal")
  
  expect_true(is.numeric(res))
  expect_length(res, 3L)
  expect_named(res, c("est", "lci", "uci"))
})


test_that("normal CI: brier matches point estimate", {
  res <- brierScore(resp, pred, conf.level = 0.95, method = "normal")
  expect_equal(unname(res["est"]), brierScore(resp, pred))
})


test_that("normal CI: lci < brier < uci", {
  res <- brierScore(resp, pred, conf.level = 0.95, method = "normal")
  expect_lt(res["lci"], res["est"])
  expect_lt(res["est"], res["uci"])
})


test_that("normal CI: wider at higher conf.level", {
  ci90 <- brierScore(resp, pred, conf.level = 0.90, method = "normal")
  ci99 <- brierScore(resp, pred, conf.level = 0.99, method = "normal")
  
  expect_lt(ci90["uci"] - ci90["lci"], ci99["uci"] - ci99["lci"])
})


test_that("normal CI: narrower with more data", {
  set.seed(1)
  r_small <- rbinom(50,   1, 0.4)
  p_small <- plogis(rnorm(50,  ifelse(r_small == 1, 0.5, -0.5)))
  r_large <- rbinom(2000, 1, 0.4)
  p_large <- plogis(rnorm(2000, ifelse(r_large == 1, 0.5, -0.5)))
  
  ci_small <- brierScore(r_small, p_small, conf.level = 0.95, method = "normal")
  ci_large <- brierScore(r_large, p_large, conf.level = 0.95, method = "normal")
  
  expect_lt(ci_large["uci"] - ci_large["lci"],
            ci_small["uci"] - ci_small["lci"])
})


test_that("normal CI: symmetric around brier", {
  res        <- brierScore(resp, pred, conf.level = 0.95, method = "normal")
  half_lower <- res["est"] - res["lci"]
  half_upper <- res["uci"]   - res["est"]
  expect_equal(unname(half_lower), unname(half_upper), tolerance = 1e-10)
})


# -----------------------------------------------------------------------
# Bootstrap CI
# -----------------------------------------------------------------------

test_that("boot CI: returns named numeric vector of length 3", {
  res <- brierScore(resp, pred, conf.level = 0.95, method = "boot", R = 299)
  
  expect_true(is.numeric(res))
  expect_length(res, 3L)
  expect_named(res, c("est", "lci", "uci"))
})


test_that("boot CI: brier matches point estimate", {
  res <- brierScore(resp, pred, conf.level = 0.95, method = "boot", R = 299)
  expect_equal(unname(res["est"]), brierScore(resp, pred))
})


test_that("boot CI: lci < brier < uci", {
  res <- brierScore(resp, pred, conf.level = 0.95, method = "boot", R = 499)
  expect_lt(res["lci"], res["est"])
  expect_lt(res["est"], res["uci"])
})


test_that("boot CI: perc type works", {
  res <- brierScore(resp, pred, conf.level = 0.95, method = "boot",
                    type = "perc", R = 299)
  expect_named(res, c("est", "lci", "uci"))
  expect_lt(res["lci"], res["est"])
})


test_that("boot CI: norm type works", {
  res <- brierScore(resp, pred, conf.level = 0.95, method = "boot",
                    type = "norm", R = 299)
  expect_named(res, c("est", "lci", "uci"))
  expect_lt(res["lci"], res["est"])
})


test_that("boot CI: bca and perc agree for large n", {
  set.seed(1)
  r <- rbinom(500, 1, 0.4)
  p <- plogis(rnorm(500, ifelse(r == 1, 0.5, -0.5)))
  
  bca  <- brierScore(r, p, conf.level = 0.95, method = "boot",
                     type = "bca",  R = 999)
  perc <- brierScore(r, p, conf.level = 0.95, method = "boot",
                     type = "perc", R = 999)
  
  expect_equal(bca["lci"], perc["lci"], tolerance = 0.01)
  expect_equal(bca["uci"], perc["uci"], tolerance = 0.01)
})


test_that("boot CI: normal and boot agree for large n", {
  set.seed(2)
  r <- rbinom(1000, 1, 0.4)
  p <- plogis(rnorm(1000, ifelse(r == 1, 0.5, -0.5)))
  
  ci_norm <- brierScore(r, p, conf.level = 0.95, method = "normal")
  ci_boot <- brierScore(r, p, conf.level = 0.95, method = "boot",
                        type = "perc", R = 999)
  
  expect_equal(ci_norm["lci"], ci_boot["lci"], tolerance = 0.01)
  expect_equal(ci_norm["uci"], ci_boot["uci"], tolerance = 0.01)
})


# -----------------------------------------------------------------------
# Sides
# -----------------------------------------------------------------------

test_that("normal CI: sides = 'left' sets uci = Inf", {
  res <- brierScore(resp, pred, conf.level = 0.95, method = "normal",
                    sides = "left")
  expect_equal(unname(res["uci"]), Inf)
  expect_false(is.infinite(res["lci"]))
})


test_that("normal CI: sides = 'right' sets lci = -Inf", {
  res <- brierScore(resp, pred, conf.level = 0.95, method = "normal",
                    sides = "right")
  expect_equal(unname(res["lci"]), -Inf)
  expect_false(is.infinite(res["uci"]))
})


test_that("boot CI: sides = 'left' sets uci = Inf", {
  res <- brierScore(resp, pred, conf.level = 0.95, method = "boot",
                    sides = "left", type = "norm", R = 299)
  expect_equal(unname(res["uci"]), Inf)
})


test_that("boot CI: sides = 'right' sets lci = -Inf", {
  res <- brierScore(resp, pred, conf.level = 0.95, method = "boot",
                    sides = "right", type = "norm", R = 299)
  expect_equal(unname(res["lci"]), -Inf)
})


test_that("normal CI: one-sided 95% lci equals two-sided 90% lci", {
  left     <- brierScore(resp, pred, conf.level = 0.95, method = "normal",
                         sides = "left")
  twosided <- brierScore(resp, pred, conf.level = 0.90, method = "normal",
                         sides = "two.sided")
  expect_equal(unname(left["lci"]), unname(twosided["lci"]),
               tolerance = 1e-10)
})


# -----------------------------------------------------------------------
# Input validation
# -----------------------------------------------------------------------

test_that("non-binary response raises error", {
  expect_error(brierScore(c(0, 1, 2), c(0.1, 0.5, 0.9)), "binary")
})


test_that("pred outside [0,1] raises error", {
  expect_error(brierScore(c(0L, 1L), c(0.5, 1.5)), "probabilities")
})


test_that("length mismatch raises error", {
  expect_error(brierScore(c(0L, 1L, 0L), c(0.1, 0.9)), "same length")
})


test_that("invalid conf.level raises error", {
  expect_error(brierScore(resp, pred, conf.level = 0),   "conf.level")
  expect_error(brierScore(resp, pred, conf.level = 1.5), "conf.level")
})


test_that("invalid method raises error", {
  expect_error(brierScore(resp, pred, conf.level = 0.95, method = "bca"),
               "arg")
})