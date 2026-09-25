

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

# The open side is reported at the boundary of the score's range: [0, 1]
# for the raw score, (-Inf, 1] for the scaled one (formerly +/-Inf always).

test_that("normal CI: sides = 'left' opens uci to 1", {
  res <- brierScore(resp, pred, conf.level = 0.95, method = "normal",
                    sides = "left")
  expect_equal(unname(res["uci"]), 1)
  expect_false(is.infinite(res["lci"]))
})


test_that("normal CI: sides = 'right' opens lci to 0", {
  res <- brierScore(resp, pred, conf.level = 0.95, method = "normal",
                    sides = "right")
  expect_equal(unname(res["lci"]), 0)
  expect_false(is.infinite(res["uci"]))
})


test_that("boot CI: sides = 'left' opens uci to 1", {
  res <- brierScore(resp, pred, conf.level = 0.95, method = "boot",
                    sides = "left", type = "norm", R = 299)
  expect_equal(unname(res["uci"]), 1)
})


test_that("boot CI: sides = 'right' opens lci to 0", {
  res <- brierScore(resp, pred, conf.level = 0.95, method = "boot",
                    sides = "right", type = "norm", R = 299)
  expect_equal(unname(res["lci"]), 0)
})


test_that("the scaled score opens downwards to -Inf", {
  r <- brierScore(resp, pred, conf.level = 0.95, scaled = TRUE,
                  sides = "right")
  l <- brierScore(resp, pred, conf.level = 0.95, scaled = TRUE,
                  sides = "left")
  expect_identical(unname(r[["lci"]]), -Inf)
  expect_equal(unname(l[["uci"]]), 1)
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


# Additional coverage: explicit branches and reference results
test_that("brier_boot_cpp resamples paired outcomes and predictions", {
  boot <- DescToolsX:::brier_boot_cpp
  y <- rep(c(0, 1), 10)
  set.seed(401)
  expect_equal(as.numeric(boot(y, y, R = 25L, scaled = FALSE)), rep(0, 25))
  expect_equal(as.numeric(boot(y, 1 - y, R = 25L, scaled = FALSE)), rep(1, 25))
  expect_equal(as.numeric(boot(y, rep(0.5, length(y)), R = 25L, scaled = FALSE)),
               rep(0.25, 25))
  expect_length(boot(y, y, R = 1L, scaled = FALSE), 1L)
})

test_that("brier_boot_cpp scales by the prevalence of each resample", {
  boot <- DescToolsX:::brier_boot_cpp
  y <- rep(c(0, 0, 1), 20)
  p <- numeric(length(y))
  # With p=0 the unscaled Brier score is exactly the resampled prevalence.
  set.seed(402)
  prevalence <- as.numeric(boot(y, p, R = 80L, scaled = FALSE))
  expect_true(all(prevalence > 0 & prevalence < 1))
  set.seed(402)
  scaled <- as.numeric(boot(y, p, R = 80L, scaled = TRUE))
  expect_equal(scaled, 1 - prevalence / (prevalence * (1 - prevalence)))
  set.seed(402)
  expect_identical(scaled, as.numeric(boot(y, p, R = 80L, scaled = TRUE)))
})


# Review 25.09.2026 ------------------------------------------------------------

test_that("conf.level and scaled are validated before use", {
  for (cl in list(NULL, NaN, c(0.9, 0.95), "0.95"))
    expect_error(brierScore(resp, pred, conf.level = cl), "conf.level")
  expect_error(brierScore(resp, pred, scaled = NA), "scaled")
  expect_error(brierScore(resp, pred, conf.level = 0.4, sides = "left"),
               "exceed 0.5")
})

test_that("the scaled score of a constant response is an error, not -Inf", {
  expect_error(brierScore(c(0, 0, 0), c(0.1, 0.2, 0.3), scaled = TRUE),
               "no variation")
})

test_that("perc and basic bootstrap bounds come from the same replicates", {
  R <- 500L
  set.seed(11)
  v <- as.numeric(DescToolsX:::brier_boot_cpp(resp, pred, R, FALSE))
  est <- brierScore(resp, pred)

  set.seed(11)
  p <- brierScore(resp, pred, conf.level = 0.9, method = "boot",
                  type = "perc", R = R)
  expect_equal(unname(p[c("lci", "uci")]),
               quantile(v, c(0.05, 0.95), names = FALSE))

  set.seed(11)
  b <- brierScore(resp, pred, conf.level = 0.9, method = "boot",
                  type = "basic", R = R)
  expect_equal(unname(b[c("lci", "uci")]),
               2 * est - quantile(v, c(0.95, 0.05), names = FALSE))

  expect_error(brierScore(resp, pred, conf.level = 0.9, method = "boot",
                          type = "stud", R = R), "stud")
})

test_that("resamples with a constant response are dropped with a warning", {
  set.seed(12)
  v <- as.numeric(DescToolsX:::brier_boot_cpp(c(0, 1), c(0.2, 0.8), 200L, TRUE))
  expect_true(anyNA(v))
  expect_true(all(is.finite(v[!is.na(v)])))

  set.seed(13)
  r <- c(0, 0, 0, 1, 0, 1)
  expect_warning(res <- brierScore(r, c(0.1, 0.3, 0.2, 0.7, 0.4, 0.6),
                                   conf.level = 0.9, scaled = TRUE,
                                   method = "boot", type = "perc", R = 400),
                 "constant response")
  expect_true(all(is.finite(res)))
})

test_that("the glm method takes fitted probabilities and the response", {
  fit <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_equal(brierScore(fit), brierScore(mtcars$vs, fitted(fit)))
  fit0 <- glm(vs ~ mpg, data = mtcars, family = binomial, y = FALSE)
  expect_equal(brierScore(fit0), brierScore(fit))
})
