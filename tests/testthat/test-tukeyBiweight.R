# ============================================================
# test-tukeyBiweight.R
# ============================================================

# -----------------------------------------------------------------------
# Point estimate (conf.level = NA)
# -----------------------------------------------------------------------

test_that("point estimate: returns single numeric", {
  set.seed(1)
  x   <- rnorm(50)
  res <- tukeyBiweight(x)
  
  expect_true(is.numeric(res))
  expect_length(res, 1L)
})


test_that("point estimate: close to mean for normal data", {
  set.seed(2)
  x <- rnorm(500)
  
  expect_equal(tukeyBiweight(x), mean(x), tolerance = 0.05)
})


test_that("point estimate: robust against outliers", {
  set.seed(3)
  x_clean   <- rnorm(100)
  x_outlier <- c(x_clean, rep(1000, 5))  # same base data + outliers
  
  est_clean   <- tukeyBiweight(x_clean)
  est_outlier <- tukeyBiweight(x_outlier)
  
  # biweight estimate stays in a plausible range despite extreme outliers
  expect_lt(abs(est_outlier), 1.0)   # not pulled toward 1000
  
  # mean is not robust — it shifts substantially
  expect_gt(abs(mean(x_outlier) - mean(x_clean)), 40)
})


test_that("point estimate: const parameter changes result", {
  set.seed(4)
  x <- c(rnorm(50), 5, -5)
  
  est_9  <- tukeyBiweight(x, const = 9)
  est_3  <- tukeyBiweight(x, const = 3)
  
  # smaller const = more aggressive downweighting → different estimate
  expect_false(isTRUE(all.equal(est_9, est_3)))
})


test_that("point estimate: single-element vector", {
  expect_equal(tukeyBiweight(42), 42)
})


# -----------------------------------------------------------------------
# NA handling
# -----------------------------------------------------------------------

test_that("na.rm = FALSE returns NA when NAs present", {
  x <- c(1, 2, NA, 4)
  expect_true(is.na(tukeyBiweight(x, na.rm = FALSE)))
})


test_that("na.rm = TRUE removes NAs and computes correctly", {
  set.seed(1)
  x    <- rnorm(50)
  xna  <- x
  xna[c(3, 17, 42)] <- NA
  
  expect_equal(tukeyBiweight(xna, na.rm = TRUE),
               tukeyBiweight(x[-c(3, 17, 42)]))
})


# -----------------------------------------------------------------------
# CI: structure
# -----------------------------------------------------------------------

test_that("CI: returns named numeric vector of length 3", {
  set.seed(1)
  x   <- rnorm(50)
  res <- tukeyBiweight(x, conf.level = 0.95)
  
  expect_true(is.numeric(res))
  expect_length(res, 3L)
  expect_named(res, c("est", "lci", "uci"))
})


test_that("CI: est matches point estimate", {
  set.seed(1)
  x   <- rnorm(100)
  res <- tukeyBiweight(x, conf.level = 0.95)
  
  expect_equal(unname(res["est"]), tukeyBiweight(x))
})


test_that("CI: lci < est < uci", {
  set.seed(5)
  x   <- rnorm(100)
  res <- tukeyBiweight(x, conf.level = 0.95)
  
  expect_lt(res["lci"], res["est"])
  expect_lt(res["est"], res["uci"])
})


test_that("CI: wider at higher conf.level", {
  set.seed(6)
  x    <- rnorm(100)
  ci90 <- tukeyBiweight(x, conf.level = 0.90)
  ci99 <- tukeyBiweight(x, conf.level = 0.99)
  
  width90 <- ci90["uci"] - ci90["lci"]
  width99 <- ci99["uci"] - ci99["lci"]
  
  expect_lt(width90, width99)
})


test_that("CI: narrower with more data", {
  set.seed(7)
  ci_small <- tukeyBiweight(rnorm(30),  conf.level = 0.95)
  ci_large <- tukeyBiweight(rnorm(500), conf.level = 0.95)
  
  expect_lt(ci_large["uci"] - ci_large["lci"],
            ci_small["uci"] - ci_small["lci"])
})


test_that("CI: perc and bca give similar results for large n", {
  set.seed(8)
  x    <- rnorm(300)
  perc <- tukeyBiweight(x, conf.level = 0.95, type = "perc", R = 999)
  bca  <- tukeyBiweight(x, conf.level = 0.95, type = "bca",  R = 999)
  
  expect_equal(unname(perc["est"]), unname(bca["est"]))
  expect_equal(perc["lci"], bca["lci"], tolerance = 0.1)
  expect_equal(perc["uci"], bca["uci"], tolerance = 0.1)
})


# -----------------------------------------------------------------------
# CI: sides
# -----------------------------------------------------------------------

test_that("CI: sides = 'left' sets uci = Inf", {
  set.seed(1)
  res <- tukeyBiweight(rnorm(50), conf.level = 0.95, sides = "left")
  expect_equal(unname(res["uci"]), Inf)
  expect_false(is.infinite(res["lci"]))
})


test_that("CI: sides = 'right' sets lci = -Inf", {
  set.seed(1)
  res <- tukeyBiweight(rnorm(50), conf.level = 0.95, sides = "right")
  expect_equal(unname(res["lci"]), -Inf)
  expect_false(is.infinite(res["uci"]))
})


test_that("CI: sides = 'left' lci equals two-sided lci at adjusted level", {
  set.seed(9)
  x        <- rnorm(200)
  # one-sided 95% ≡ two-sided 90% — same C++ seed ensures identical resamples
  left     <- tukeyBiweight(x, conf.level = 0.95, sides = "left",
                            R = 999, seed = 1L)
  twosided <- tukeyBiweight(x, conf.level = 0.90, sides = "two.sided",
                            R = 999, seed = 1L)
  
  expect_equal(unname(left["lci"]), unname(twosided["lci"]), tolerance = 0.02)
})


# -----------------------------------------------------------------------
# CI: seed reproducibility
# -----------------------------------------------------------------------

test_that("CI: results are reproducible with explicit seed", {
  x  <- rnorm(80)
  set.seed(42)
  r1 <- tukeyBiweight(x, conf.level = 0.95, R = 499)
  set.seed(42)
  r2 <- tukeyBiweight(x, conf.level = 0.95, R = 499)
  
  expect_identical(r1, r2)
})



# -----------------------------------------------------------------------
# Input validation
# -----------------------------------------------------------------------

test_that("non-numeric x raises error", {
  expect_error(tukeyBiweight(letters[1:5]), "non-empty numeric")
})


test_that("empty vector raises error", {
  expect_error(tukeyBiweight(numeric(0)), "non-empty numeric")
})


test_that("invalid conf.level raises error", {
  expect_error(tukeyBiweight(rnorm(10), conf.level = 1.5), "conf.level")
  expect_error(tukeyBiweight(rnorm(10), conf.level = 0),   "conf.level")
})

