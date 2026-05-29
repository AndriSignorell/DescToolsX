
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

