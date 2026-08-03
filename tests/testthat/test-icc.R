
# Shrout & Fleiss (1979) example data
.sf <- matrix(c(9,2,5,8, 6,1,3,2, 8,4,6,8, 7,1,2,6, 10,5,6,9, 6,2,4,7),
              ncol = 4, byrow = TRUE,
              dimnames = list(paste0("S",1:6), paste0("J",1:4)))

test_that("icc returns a single numeric by default (twoway, agreement, single)", {
  res <- icc(.sf)
  expect_length(res, 1)
  expect_true(is.numeric(res))
})

test_that("icc result is in [-1, 1]", {
  res <- icc(.sf)
  expect_gte(res, -1)
  expect_lte(res,  1)
})

test_that("icc oneway agreement single ≈ 0.17 for Shrout-Fleiss data (ICC(1,1))", {
  # Shrout & Fleiss Table 2: ICC(1,1) reported as 0.17 (rounded);
  # exact computed value is ≈ 0.166 — allow 3% relative tolerance
  expect_equal(icc(.sf, model = "oneway", type = "agreement", unit = "single"),
               0.17, tolerance = 0.03)
})

test_that("icc twoway agreement single ≈ 0.29 for Shrout-Fleiss data (ICC(2,1))", {
  expect_equal(icc(.sf, model = "twoway", type = "agreement", unit = "single"),
               0.29, tolerance = 0.02)
})

test_that("icc model = 'oneway' returns a value in [-1, 1]", {
  res <- icc(.sf, model = "oneway")
  expect_gte(res, -1); expect_lte(res, 1)
})

test_that("icc unit = 'average' gives higher ICC than unit = 'single'", {
  single  <- icc(.sf, unit = "single")
  average <- icc(.sf, unit = "average")
  expect_gt(average, single)
})

test_that("icc type = 'consistency' differs from type = 'agreement'", {
  agree <- icc(.sf, type = "agreement")
  cons  <- icc(.sf, type = "consistency")
  expect_false(isTRUE(all.equal(agree, cons)))
})

test_that("icc conf.level returns named vector est/lci/uci", {
  res <- icc(.sf, conf.level = 0.95)
  expect_length(res, 3)
  expect_named(res, c("est", "lci", "uci"))
})

test_that("icc CI: lci < est < uci", {
  res <- icc(.sf, conf.level = 0.95)
  expect_lt(res["lci"], res["est"])
  expect_gt(res["uci"], res["est"])
})

test_that("icc method = 'boot' returns a 3-element vector with finite CI", {
  res <- icc(.sf, conf.level = 0.95, method = "boot", R = 100)
  expect_length(res, 3)
  expect_named(res, c("est", "lci", "uci"))
  expect_true(is.finite(res["lci"]))
  expect_true(is.finite(res["uci"]))
})

test_that("icc na.rm = TRUE works when data has NA", {
  sf_na <- .sf
  sf_na[1, 1] <- NA
  res <- icc(sf_na, na.rm = TRUE)
  expect_true(is.numeric(res))
})




test_that("icc refuses to invent a REML confidence interval", {
  
  sf <- matrix(c(9,2,5,8, 6,1,3,2, 8,4,6,8, 7,1,2,6, 10,5,6,9, 6,2,4,7),
               ncol = 4, byrow = TRUE)
  
  skip_if_not_installed("lme4")
  
  # the point estimate is fine
  expect_true(is.finite(icc(sf, method = "reml")))
  
  # the interval used a hard-coded se of 1/sqrt(50), i.e. the same width
  # for ns = 10 and ns = 10000
  expect_error(icc(sf, method = "reml", conf.level = 0.95),
               "not implemented")
})


test_that("REML honours type and unit", {
  
  skip_if_not_installed("lme4")
  
  set.seed(1)
  ns <- 30; nr <- 4
  subj <- rnorm(ns, sd = 2)
  rater <- rnorm(nr, sd = 1)
  sf <- outer(subj, rater, "+") + matrix(rnorm(ns * nr), ns, nr)
  
  single <- icc(sf, method = "reml", unit = "single")
  avg    <- icc(sf, method = "reml", unit = "average")
  
  # the average form was silently identical to the single one
  expect_gt(avg, single)
  
  agree <- icc(sf, method = "reml", type = "agreement")
  consi <- icc(sf, method = "reml", type = "consistency")
  
  # consistency drops the rater variance from the denominator
  expect_gt(consi, agree)
  
  # and both stay close to their ANOVA counterparts
  expect_equal(icc(sf, method = "reml", type = "consistency"),
               icc(sf, method = "anova", type = "consistency"),
               tolerance = 0.05)
})


test_that("oneway plus consistency is refused, not returned as NULL", {
  
  sf <- matrix(c(9,2,5,8, 6,1,3,2, 8,4,6,8, 7,1,2,6, 10,5,6,9, 6,2,4,7),
               ncol = 4, byrow = TRUE)
  
  expect_error(icc(sf, model = "oneway", type = "consistency"),
               "not defined")
  expect_error(icc(sf, model = "oneway", type = "consistency",
                   conf.level = 0.95), "not defined")
})


test_that("icc reproduces the Shrout and Fleiss reference values", {
  
  sf <- matrix(c(9,2,5,8, 6,1,3,2, 8,4,6,8, 7,1,2,6, 10,5,6,9, 6,2,4,7),
               ncol = 4, byrow = TRUE)
  
  # Shrout & Fleiss (1979), Table 4. The paper reports two decimals, so
  # compare at that resolution - expect_equal()'s tolerance is RELATIVE,
  # and 0.1657 against 0.17 is 2.5%, which 0.005 does not cover. Same
  # mistake I made with the PSU kappa reference.
  expect_equal(round(icc(sf, model = "oneway", type = "agreement",
                         unit = "single"), 2), 0.17)
  expect_equal(round(icc(sf, model = "twoway", type = "agreement",
                         unit = "single"), 2), 0.29)
  expect_equal(round(icc(sf, model = "twoway", type = "consistency",
                         unit = "single"), 2), 0.71)
  
  ci <- icc(sf, model = "twoway", type = "consistency",
            unit = "single", conf.level = 0.95)
  expect_named(ci, c("est", "lci", "uci"))
  expect_lt(ci[["lci"]], ci[["est"]])
  expect_gt(ci[["uci"]], ci[["est"]])
})

