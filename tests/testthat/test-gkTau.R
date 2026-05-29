.tab <- as.table(rbind(c(26,26,23,18,9), c(6,7,9,14,23)))

test_that("gkTau returns a single numeric (no CI)", {
  res <- gkTau(.tab)
  expect_length(res, 1)
  expect_true(is.numeric(res))
})

test_that("gkTau result is in [0, 1]", {
  res <- gkTau(.tab)
  expect_gte(res, 0); expect_lte(res, 1)
})

test_that("gkTau direction = 'row' and 'column' give different values", {
  r <- gkTau(.tab, direction = "row")
  c <- gkTau(.tab, direction = "column")
  expect_false(isTRUE(all.equal(r, c)))
})

test_that("gkTau is 0 for a perfectly independent table", {
  tab <- as.table(matrix(rep(25,4), nrow=2))
  expect_equal(gkTau(tab), 0, tolerance = 1e-8)
})

test_that("gkTau reduces to phi^2 for a 2x2 table", {
  tab <- as.table(cbind(c(11,2), c(4,6)))
  tau_r <- gkTau(tab, direction = "row")
  tau_c <- gkTau(tab, direction = "column")
  phi2  <- chisq.test(tab, correct=FALSE)$statistic / sum(tab)
  expect_equal(tau_r, unname(phi2), tolerance = 0.001)
  expect_equal(tau_c, unname(phi2), tolerance = 0.001)
})

test_that("gkTau conf.level returns 3-element named vector", {
  res <- gkTau(.tab, conf.level = 0.95)
  expect_length(res, 3)
  expect_named(res, c("tauA","lwr.ci","upr.ci"))
})

test_that("gkTau CI: lwr.ci <= tauA <= upr.ci", {
  res <- gkTau(.tab, conf.level = 0.95)
  expect_lte(res["lwr.ci"], res["tauA"])
  expect_gte(res["upr.ci"], res["tauA"])
})

test_that("gkTau accepts two vectors", {
  x <- c(1,2,2,3); y <- c(1,1,2,3)
  expect_length(gkTau(x, y), 1)
})
