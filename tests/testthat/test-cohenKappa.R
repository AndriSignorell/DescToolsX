
# ============================================================
# test-cohenKappa.R
# ============================================================

# shared test matrices
m3 <- matrix(c(53,  5, 2,
               11, 14, 5,
               1,  6, 3),
             nrow = 3, byrow = TRUE,
             dimnames = list(rater1 = c("V","N","P"),
                             rater2 = c("V","N","P")))

# Fleiss, Cohen & Everitt (1969) reference matrix
fleiss <- matrix(c(106, 10,  4,
                   22, 28, 10,
                   2, 12,  6),
                 nrow = 3, byrow = TRUE)

# 2x2 for simple checks
m2 <- matrix(c(40, 10,
               8, 42),
             nrow = 2, byrow = TRUE)


# -----------------------------------------------------------------------
# Point estimate — unweighted
# -----------------------------------------------------------------------

test_that("unweighted: returns single numeric", {
  res <- cohenKappa(m3)
  expect_true(is.numeric(res))
  expect_length(res, 1L)
})


test_that("unweighted: perfect agreement gives kappa = 1", {
  m <- diag(c(20, 30, 10))
  expect_equal(cohenKappa(m), 1)
})


test_that("unweighted: correct value for Bortz matrix", {
  # manually: po = (53+14+3)/100 = 0.70, pc from marginals
  n      <- sum(m3)
  p      <- m3 / n
  po     <- sum(diag(p))
  pc     <- sum(colSums(p) * rowSums(p))
  k_exp  <- (po - pc) / (1 - pc)
  expect_equal(cohenKappa(m3), k_exp)
})


test_that("unweighted: kappa in (-1, 1] for typical data", {
  res <- cohenKappa(m3)
  expect_gt(res, -1)
  expect_lte(res, 1)
})


test_that("unweighted: 2x2 matrix", {
  res <- cohenKappa(m2)
  expect_true(is.numeric(res))
  expect_gt(res, 0)
})


# -----------------------------------------------------------------------
# Point estimate — weighted
# -----------------------------------------------------------------------

test_that("equal-spacing: returns numeric scalar", {
  res <- cohenKappa(m3, weights = "equal-spacing")
  expect_true(is.numeric(res))
  expect_length(res, 1L)
})


test_that("equal-spacing >= unweighted for ordered categories", {
  # weighted kappa is generally >= unweighted when weights penalise
  # distant disagreements less than 1
  expect_gte(cohenKappa(m3, weights = "equal-spacing"),
             cohenKappa(m3, weights = "unweighted"))
})


test_that("fleiss-cohen: returns numeric scalar", {
  res <- cohenKappa(fleiss, weights = "fleiss-cohen")
  expect_true(is.numeric(res))
  expect_length(res, 1L)
})


test_that("fleiss-cohen >= equal-spacing (quadratic > linear penalty)", {
  # Fleiss-Cohen weights decrease more slowly from the diagonal →
  # closer disagreements penalised less → higher weighted kappa
  expect_gte(cohenKappa(m3, weights = "fleiss-cohen"),
             cohenKappa(m3, weights = "equal-spacing"))
})


test_that("user weight matrix: identity weights equal unweighted", {
  W   <- diag(ncol(m3))
  k_w <- cohenKappa(m3, weights = W)
  k_u <- cohenKappa(m3, weights = "unweighted")
  expect_equal(k_w, k_u)
})


test_that("user weight matrix: equal-spacing matches built-in", {
  nc <- ncol(m3)
  W  <- outer(seq_len(nc), seq_len(nc),
              function(i, j) 1 - abs(i - j) / (nc - 1))
  expect_equal(cohenKappa(m3, weights = W),
               cohenKappa(m3, weights = "equal-spacing"))
})


# -----------------------------------------------------------------------
# CI — structure and properties
# -----------------------------------------------------------------------

test_that("CI: returns named numeric vector of length 3", {
  res <- cohenKappa(m3, conf.level = 0.95)
  
  expect_true(is.numeric(res))
  expect_length(res, 3L)
  expect_named(res, c("est", "lci", "uci"))
})


test_that("CI: est matches point estimate", {
  res <- cohenKappa(m3, conf.level = 0.95)
  expect_equal(unname(res["est"]), cohenKappa(m3))
})


test_that("CI: lci < est < uci", {
  res <- cohenKappa(m3, conf.level = 0.95)
  expect_lt(res["lci"], res["est"])
  expect_lt(res["est"], res["uci"])
})


test_that("CI: wider at higher conf.level", {
  ci90 <- cohenKappa(m3, conf.level = 0.90)
  ci99 <- cohenKappa(m3, conf.level = 0.99)
  
  expect_lt(ci90["uci"] - ci90["lci"],
            ci99["uci"] - ci99["lci"])
})


test_that("CI: narrower with more data (scaled-up matrix)", {
  ci_small <- cohenKappa(m3,       conf.level = 0.95)
  ci_large <- cohenKappa(m3 * 10L, conf.level = 0.95)
  
  expect_equal(unname(ci_small["est"]), unname(ci_large["est"]))
  expect_lt(ci_large["uci"] - ci_large["lci"],
            ci_small["uci"] - ci_small["lci"])
})


test_that("CI: symmetric around est (Wald)", {
  res        <- cohenKappa(m3, conf.level = 0.95)
  half_lower <- res["est"] - res["lci"]
  half_upper <- res["uci"] - res["est"]
  expect_equal(unname(half_lower), unname(half_upper), tolerance = 1e-10)
})


test_that("weighted CI: lci < est < uci", {
  res <- cohenKappa(m3, weights = "equal-spacing", conf.level = 0.95)
  expect_lt(res["lci"], res["est"])
  expect_lt(res["est"], res["uci"])
})


# -----------------------------------------------------------------------
# Sides
# -----------------------------------------------------------------------

test_that("sides = 'left' sets uci = Inf", {
  res <- cohenKappa(m3, conf.level = 0.95, sides = "left")
  expect_equal(unname(res["uci"]), Inf)
  expect_false(is.infinite(res["lci"]))
})


test_that("sides = 'right' sets lci = -Inf", {
  res <- cohenKappa(m3, conf.level = 0.95, sides = "right")
  expect_equal(unname(res["lci"]), -Inf)
  expect_false(is.infinite(res["uci"]))
})


test_that("sides = 'left' lci equals two-sided 90% lci", {
  left     <- cohenKappa(m3, conf.level = 0.95, sides = "left")
  twosided <- cohenKappa(m3, conf.level = 0.90, sides = "two.sided")
  expect_equal(unname(left["lci"]), unname(twosided["lci"]),
               tolerance = 1e-10)
})


# -----------------------------------------------------------------------
# Vector interface
# -----------------------------------------------------------------------

test_that("vector interface matches matrix interface", {
  x  <- bedrock::untable(m3)
  kv <- cohenKappa(x$rater1, x$rater2)
  km <- cohenKappa(m3)
  expect_equal(kv, km)
})


test_that("vector interface with CI matches matrix interface", {
  x   <- bedrock::untable(m3)
  resv <- cohenKappa(x$rater1, x$rater2, conf.level = 0.95)
  resm <- cohenKappa(m3, conf.level = 0.95)
  expect_equal(resv, resm)
})


test_that("vector interface + weighted raises error", {
  x <- bedrock::untable(m3)
  expect_error(
    cohenKappa(x$rater1, x$rater2, weights = "equal-spacing"),
    "unweighted kappa only"
  )
})


# -----------------------------------------------------------------------
# Input validation
# -----------------------------------------------------------------------

test_that("invalid conf.level raises error", {
  expect_error(cohenKappa(m3, conf.level = 0),   "conf.level")
  expect_error(cohenKappa(m3, conf.level = 1.5), "conf.level")
})


test_that("vector conf.level raises error", {
  expect_error(cohenKappa(m3, conf.level = c(0.90, 0.95)), "single value")
})


test_that("invalid weights string raises error", {
  expect_error(cohenKappa(m3, weights = "quadratic"), "arg")
})


test_that("weight matrix: non-numeric raises error", {
  W <- matrix(as.character(diag(3)), 3, 3)
  expect_error(cohenKappa(m3, weights = W), "numeric")
})


test_that("weight matrix: wrong dimensions raises error", {
  W <- diag(4)   # m3 is 3x3
  expect_error(cohenKappa(m3, weights = W), "dimensions")
})


test_that("weight matrix: non-finite values raise error", {
  W      <- diag(3)
  W[1,2] <- Inf
  expect_error(cohenKappa(m3, weights = W), "NA, NaN, or Inf")
})


test_that("weight matrix: values outside [0,1] give warning", {
  W      <- diag(3)
  W[1,2] <- 1.5
  W[2,1] <- 1.5
  expect_warning(cohenKappa(m3, weights = W), "outside")
})


test_that("asymmetric weight matrix gives warning", {
  W      <- diag(3)
  W[1,2] <- 0.5
  W[2,1] <- 0.3
  expect_warning(cohenKappa(m3, weights = W), "symmetric")
})


test_that("empty confusion matrix raises error", {
  m0 <- matrix(0L, 3, 3)
  expect_error(cohenKappa(m0), "empty")
})


test_that("degenerate marginals (pc ≈ 1) raise error", {
  # All observations on one diagonal → colFreqs and rowFreqs both
  # concentrated on one category → pc ≈ 1
  m_deg <- matrix(c(1000, 0, 0,
                    0, 0, 0,
                    0, 0, 0), nrow = 3)
  expect_error(cohenKappa(m_deg), "too close to 1")
})