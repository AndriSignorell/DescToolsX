
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




test_that("huberM's Wald interval uses the scale it was given", {
  
  set.seed(7)
  x <- c(round(rnorm(200), 1), round(rnorm(10, mean = 10, sd = 10)))
  
  # With the default mu and s the two routes agree, which is why the bug
  # was invisible: .tauHuber()'s default s = mad(x) equals
  # mad(x, center = median(x)).
  a <- huberM(x, conf.level = 0.95)
  expect_named(a, c("est", "lci", "uci"))
  
  # Supplying s explicitly used to leave tau standardized by mad(x) while
  # the interval was scaled by s. Doubling s must widen the interval by
  # the same factor, since the estimate is unchanged when s only rescales
  # the winsorizing window symmetrically.
  b <- huberM(x, conf.level = 0.95, s = mad(x))
  expect_equal(unname(b[["uci"]] - b[["lci"]]),
               unname(a[["uci"]] - a[["lci"]]), tolerance = 1e-8)
  
  # the interval brackets the estimate
  expect_lt(a[["lci"]], a[["est"]])
  expect_gt(a[["uci"]], a[["est"]])
})


test_that("huberM survives a zero scale", {
  
  expect_equal(suppressWarnings(huberM(rep(9, 100))), 9)
  expect_warning(huberM(rep(9, 100)), "zero")
})


# Additional branch coverage and reference checks
test_that("huberM handles missing interval input and explicit starting values", {
  expect_identical(huberM(c(1, NA), conf.level = 0.95),
                   c(est = NA_real_, lci = NA_real_, uci = NA_real_))
  expect_true(is.na(huberM(c(NA_real_, NA_real_), na.rm = TRUE)))
  for (bad in list("1", c(1, 2))) {
    expect_error(huberM(1:5, mu = bad), "'mu'")
    expect_error(huberM(1:5, s = bad), "'s'")
  }
  expect_equal(huberM(c(1, 2, 5), mu = 0, s = 100, k = 100), 8 / 3)
})

test_that("huberM internal weighted location matches a replicated sample", {
  engine <- DescToolsX:::.huberM
  x <- c(0, 2, 4)
  w <- c(1, 2, 1)
  a <- engine(x, weights = w, mu = 0, s = 1, k = 100)
  expect_equal(a$mu, mean(rep(x, w)))
  b <- engine(c(x, NA), weights = c(w, 7), mu = 0, s = 1, k = 100)
  expect_equal(a$mu, b$mu)
  expect_true(is.na(engine(x, weights = c(0, 0, 0), mu = 0, s = 1)$mu))
  # the standard error is available with weights now (frequency weights:
  # the replicated sample's SE), where the original stopped
  expect_equal(engine(x, weights = w, mu = 0, s = 1, se = TRUE)$SE,
               engine(rep(x, w), mu = 0, s = 1, se = TRUE)$SE)
  expect_error(engine(x, mu = 0, s = -1), "negative scale")
  expect_error(engine(x, weights = c(1, -1, 1), mu = 0, s = 1))
})

test_that("huberM normal bootstrap bounds agree with boot for an unclipped mean", {
  # The fixed clipping window contains every observation and every resample mean.
  x <- seq(-2, 2, length.out = 41)
  set.seed(812)
  actual <- huberM(x, conf.level = 0.9, method = "boot", type = "norm",
                   R = 199, mu = 0, s = 100, k = 100, parallel = "no", ncpus = 1)
  set.seed(812)
  ref <- boot::boot(x, statistic = function(z, i) mean(z[i]), R = 199,
                    sim = "ordinary", parallel = "no", ncpus = 1)
  bounds <- boot::boot.ci(ref, conf = 0.9, type = "norm")$normal[2:3]
  expect_equal(unname(actual), c(mean(x), unname(bounds)), tolerance = 1e-7)
})


# -----------------------------------------------------------------------
# Weights (formerly only reachable inside .huberM) - review 25.09.2026
# -----------------------------------------------------------------------

set.seed(41)
xh <- round(c(rnorm(30, 10, 2), 25, 31), 1)
wh <- sample(1:4, length(xh), replace = TRUE)

test_that("frequency weights reproduce the replicated data exactly", {
  xr <- rep(xh, wh)
  expect_equal(huberM(xh, weights = wh), huberM(xr))
  expect_equal(huberM(xh, weights = wh, conf.level = 0.95),
               huberM(xr, conf.level = 0.95))
  # unit weights change nothing
  expect_equal(huberM(xh, weights = rep(1, length(xh)), conf.level = 0.9),
               huberM(xh, conf.level = 0.9))
})

test_that("the weighted starting values are median and MAD of the replicates", {
  xr <- rep(xh, wh)
  mu <- medianX(xh, weights = wh)
  expect_equal(mu, median(xr))
  expect_equal(1.4826 * medianX(abs(xh - mu), weights = wh),
               mad(xr, center = mu))
  expect_equal(.tauHuber(xh, mu = mu, s = mad(xr), weights = wh),
               .tauHuber(xr, mu = mu, s = mad(xr)))
})

test_that("missing values drop together with their weights", {
  expect_equal(huberM(c(xh, NA), weights = c(wh, 7), na.rm = TRUE),
               huberM(xh, weights = wh))
  expect_true(is.na(huberM(c(xh, NA), weights = c(wh, 7))))
})

test_that("the weighted bootstrap resamples observations with their weights", {
  set.seed(1)
  res <- huberM(xh, weights = wh, conf.level = 0.9, method = "boot",
                R = 499, type = "perc")
  expect_named(res, c("est", "lci", "uci"))
  expect_true(res[["lci"]] <= res[["est"]] && res[["est"]] <= res[["uci"]])
  expect_equal(unname(res[["est"]]), huberM(xh, weights = wh))
  set.seed(1)
  expect_no_error(huberM(xh, conf.level = 0.9, method = "boot", R = 499,
                         type = "basic"))
})

test_that("sides and conf.level follow the suite conventions", {
  two <- huberM(xh, conf.level = 0.90)
  l <- huberM(xh, conf.level = 0.95, sides = "left")
  expect_equal(unname(l[["lci"]]), unname(two[["lci"]]))
  expect_identical(unname(l[["uci"]]), Inf)
  expect_identical(unname(huberM(xh, conf.level = 0.95,
                                 sides = "right")[["lci"]]), -Inf)
  expect_error(huberM(xh, conf.level = 0.4, sides = "left"), "exceed 0.5")
  for (cl in list(NULL, NaN, c(0.9, 0.95)))
    expect_error(huberM(xh, conf.level = cl), "conf.level")
  expect_error(huberM(xh, na.rm = NA), "na.rm")
})

test_that("invalid weights are refused", {
  expect_error(huberM(xh, weights = wh[-1]), "weights")
  expect_error(huberM(xh, weights = replace(wh, 1, -1)), "weights")
  expect_error(huberM(xh, weights = replace(wh, 1, NA)), "weights")
  expect_error(huberM(xh, weights = rep(0, length(xh))), "sum to zero")
})
