# Fleiss (1971) example
.kappa_dat <- data.frame(
  A=c(2,3,1,3,1,2,1,2,3,3,3,3,3,2,1,3,3,2,2,1,
      2,1,3,3,2,2,1,2,1,1,2,3,3,3,3,3,1,2,1,1),
  B=c(2,2,2,1,1,2,1,2,3,3,2,3,1,3,1,1,3,2,1,2,
      2,1,3,2,2,2,3,2,1,1,2,2,3,3,3,3,2,2,2,3),
  C=c(2,2,2,1,1,2,1,2,3,3,2,3,3,3,3,2,2,2,2,3,
      2,2,3,3,2,2,3,2,2,2,2,3,3,3,3,3,3,2,2,2)
)

test_that("kappaM returns a single numeric by default (Fleiss)", {
  res <- kappaM(.kappa_dat)
  expect_length(res, 1)
  expect_true(is.numeric(res))
})

test_that("kappaM result is in [-1, 1]", {
  res <- kappaM(.kappa_dat)
  expect_gte(res, -1); expect_lte(res, 1)
})

test_that("kappaM estimator = 'conger' returns a numeric", {
  res <- kappaM(.kappa_dat, estimator = "conger")
  expect_true(is.numeric(res))
})

test_that("kappaM estimator = 'light' returns a numeric", {
  res <- kappaM(.kappa_dat, estimator = "light")
  expect_true(is.numeric(res))
})

test_that("kappaM all three estimators return different values for this data", {
  f <- kappaM(.kappa_dat, estimator = "fleiss")
  c <- kappaM(.kappa_dat, estimator = "conger")
  expect_false(isTRUE(all.equal(f, c)))
})

test_that("kappaM perfect agreement gives kappa near 1", {
  x <- data.frame(A = 1:10, B = 1:10, C = 1:10)
  expect_equal(unname(kappaM(x)), 1, tolerance = 1e-6)
})

test_that("kappaM conf.level returns named vector est/lci/uci", {
  res <- kappaM(.kappa_dat, conf.level = 0.95)
  expect_length(res, 3)
  expect_named(res, c("est","lci","uci"))
})

test_that("kappaM CI: lci < est < uci", {
  res <- kappaM(.kappa_dat, conf.level = 0.95)
  expect_lt(res["lci"], res["est"])
  expect_gt(res["uci"], res["est"])
})

test_that("kappaM accepts a matrix input", {
  m <- as.matrix(.kappa_dat)
  res <- kappaM(m)
  expect_true(is.numeric(res))
})


test_that("kappaM names the estimator argument as such", {

  # fleiss/conger/light are three coefficients for the same quantity, not
  # three interval methods - and there is only one interval method here, so
  # 'method' does not appear at all (design_rules 4.1)
  expect_true("estimator" %in% names(formals(kappaM)))
  expect_false("method" %in% names(formals(kappaM)))

  expect_error(kappaM(.kappa_dat, estimator = "cohen"), "fleiss")
})


test_that("kappaM validates conf.level through the shared helper", {

  # four hand-written blocks with the same message stood here before
  expect_error(kappaM(.kappa_dat, conf.level = c(0.9, 0.95)), "conf.level")
  expect_error(kappaM(.kappa_dat, conf.level = NULL), "conf.level")
  expect_error(kappaM(.kappa_dat, conf.level = NaN), "conf.level")
  expect_error(kappaM(.kappa_dat, conf.level = 0), "conf.level")
  expect_error(kappaM(.kappa_dat, conf.level = 1), "conf.level")

  expect_silent(kappaM(.kappa_dat))
})


test_that("kappaM closes the open side at kappa's own range", {

  two   <- kappaM(.kappa_dat, conf.level = 0.95)
  left  <- kappaM(.kappa_dat, conf.level = 0.95, sides = "left")
  right <- kappaM(.kappa_dat, conf.level = 0.95, sides = "right")

  # kappa lies in [-1, 1]; Inf used to be reported here
  expect_equal(left[["uci"]], 1)
  expect_equal(right[["lci"]], -1)

  expect_equal(left[["est"]], two[["est"]])
  expect_gt(left[["lci"]],  two[["lci"]])
  expect_lt(right[["uci"]], two[["uci"]])

  # left(gamma) reads the same end as two.sided(2*gamma - 1)
  expect_equal(left[["lci"]],
               kappaM(.kappa_dat, conf.level = 0.90)[["lci"]])
  expect_equal(right[["uci"]],
               kappaM(.kappa_dat, conf.level = 0.90)[["uci"]])
})


test_that("kappaM refuses a one-sided interval below conf.level 0.5", {

  # the critical value would turn negative and reverse the two bounds
  expect_error(kappaM(.kappa_dat, conf.level = 0.4, sides = "left"), "0.5")
  expect_error(kappaM(.kappa_dat, conf.level = 0.5, sides = "right"), "0.5")
  expect_silent(kappaM(.kappa_dat, conf.level = 0.4))
})



test_that("kappaM never reports an interval of width zero", {
  
  x <- data.frame(A = 1:10, B = 1:10, C = 1:10)
  
  for (e in c("fleiss", "conger", "light")) {
    res <- suppressWarnings(kappaM(x, estimator = e, conf.level = 0.95))
    expect_equal(unname(res[["est"]]), 1, tolerance = 1e-6, info = e)
    # entweder ein echtes Intervall oder NA - nie lci == uci
    if (!is.na(res[["lci"]]))
      expect_true(res[["lci"]] < res[["uci"]], info = e)
    }
})

test_that("kappaM keeps the interval inside [-1, 1]", {

  # the Wald interval can leave the range on small or extreme data
  set.seed(1)
  x <- data.frame(A = sample(1:2, 6, TRUE),
                  B = sample(1:2, 6, TRUE),
                  C = sample(1:2, 6, TRUE))

  res <- suppressWarnings(kappaM(x, conf.level = 0.99))
  if (!anyNA(res)) {
    expect_gte(res[["lci"]], -1)
    expect_lte(res[["uci"]], 1)
  }
})


test_that("pairwise.complete.obs names the estimator in its message", {

  d <- .kappa_dat
  d[1, 1] <- NA

  expect_error(kappaM(d, use = "pairwise.complete.obs"), "estimator")
  expect_silent(kappaM(d, estimator = "light", use = "pairwise.complete.obs"))
})
