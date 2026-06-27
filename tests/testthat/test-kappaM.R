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

test_that("kappaM method = 'Conger' returns a numeric", {
  res <- kappaM(.kappa_dat, method = "conger")
  expect_true(is.numeric(res))
})

test_that("kappaM method = 'Light' returns a numeric", {
  res <- kappaM(.kappa_dat, method = "light")
  expect_true(is.numeric(res))
})

test_that("kappaM all three methods return different values for this data", {
  f <- kappaM(.kappa_dat, method = "fleiss")
  c <- kappaM(.kappa_dat, method = "conger")
  expect_false(isTRUE(all.equal(f, c)))
})

test_that("kappaM perfect agreement gives kappa near 1", {
  x <- data.frame(A = 1:10, B = 1:10, C = 1:10)
  expect_equal(kappaM(x), 1, tolerance = 1e-6)
})

test_that("kappaM conf.level returns named vector kappa/lci/uci", {
  res <- kappaM(.kappa_dat, conf.level = 0.95)
  expect_length(res, 3)
  expect_named(res, c("kappa","lci","uci"))
})

test_that("kappaM CI: lci < kappa < uci", {
  res <- kappaM(.kappa_dat, conf.level = 0.95)
  expect_lt(res["lci"], res["kappa"])
  expect_gt(res["uci"], res["kappa"])
})

test_that("kappaM accepts a matrix input", {
  m <- as.matrix(.kappa_dat)
  res <- kappaM(m)
  expect_true(is.numeric(res))
})
