
test_that("conf() survives class labels that are substrings of each other", {

  # "A" is a substring of "AB": grep(pos, ..., fixed = TRUE) matched both
  # and collapsed the 2x2 table to 1x1 without any error
  tab <- as.table(matrix(c(30, 5, 8, 27), nrow = 2,
                         dimnames = list(c("A", "AB"), c("A", "AB"))))

  res <- conf(tab, pos = "A")

  expect_equal(dim(res$table), c(2L, 2L))
  expect_equal(res$n, 70)
  expect_equal(res$diag, 57)
  expect_equal(unname(res$acc), 57 / 70)
})


test_that("conf() rejects an unknown positive class", {
  tab <- as.table(matrix(c(30, 5, 8, 27), nrow = 2,
                         dimnames = list(c("no", "yes"), c("no", "yes"))))
  expect_error(conf(tab, pos = "maybe"), "class labels")
})


test_that("multiclass one-vs-all collapsing is label-exact", {

  lbl <- c("1", "10", "11")
  tab <- as.table(matrix(c(20, 3, 2,
                            4, 18, 5,
                            1, 6, 21), nrow = 3, byrow = TRUE,
                         dimnames = list(lbl, lbl)))

  res <- conf(tab)

  # sensitivity of class "1" is 20 / (20 + 4 + 1)
  expect_equal(unname(res$byclass["sens", "1"]), 20 / 25)
  expect_equal(sum(res$byclass["prev", ]), 1)
})


test_that("corPart works for a single variable of interest", {

  set.seed(1)
  X <- matrix(rnorm(100 * 4), ncol = 4)
  colnames(X) <- paste0("V", 1:4)

  # diag(v) with length-1 v used to build an identity matrix of size
  # round(v) instead of a 1x1 matrix
  pc <- corPart(cor(X), x = 1, y = 3:4)

  expect_equal(dim(pc), c(1L, 1L))
  expect_equal(unname(pc[1, 1]), 1)
})


test_that("corPart agrees with the Schur complement and with cor of residuals", {

  set.seed(2)
  X <- matrix(rnorm(200 * 4), ncol = 4)
  colnames(X) <- paste0("V", 1:4)

  pc <- corPart(X, x = 1:2, y = 3:4)

  r1 <- residuals(lm(X[, 1] ~ X[, 3] + X[, 4]))
  r2 <- residuals(lm(X[, 2] ~ X[, 3] + X[, 4]))

  expect_equal(unname(pc[1, 2]), unname(cor(r1, r2)), tolerance = 1e-10)
  expect_true(isSymmetric(unname(pc)))
})


test_that("a square data matrix is not mistaken for a covariance matrix", {

  set.seed(3)
  X <- matrix(rnorm(25), nrow = 5)   # 5 observations, 5 variables
  colnames(X) <- paste0("V", 1:5)

  expect_silent(pc <- corPart(X, x = 1:2, y = 3:4))
  expect_equal(dim(pc), c(2L, 2L))
})


test_that("corPolychor is not truncated at tanh(2)", {

  # two nearly identical ordinal items: the latent correlation is close
  # to 1 and used to saturate at 0.964, the boundary of the old c(-2, 2)
  # search interval
  set.seed(4)
  z <- rnorm(400)
  a <- cut(z, breaks = c(-Inf, -0.5, 0.5, Inf))
  b <- cut(z + rnorm(400, sd = 0.05), breaks = c(-Inf, -0.5, 0.5, Inf))

  rho <- corPolychor(a, b)

  expect_gt(rho, 0.97)
  expect_lt(rho, 1)
})


test_that("standard errors require ML", {
  set.seed(5)
  a <- factor(sample(1:3, 100, replace = TRUE), ordered = TRUE)
  b <- factor(sample(1:3, 100, replace = TRUE), ordered = TRUE)

  expect_error(corPolychor(a, b, method = "two-step", se = TRUE), "ML")

  res <- corPolychor(a, b, method = "ML", se = TRUE)
  expect_s3_class(res, "Polychor")
  expect_identical(res$method, "ML")
})


test_that("contCoef puts the finite bound on the named side", {

  tab <- apply(HairEyeColor, c(1, 2), sum)
  mn  <- min(dim(tab))
  cMax <- sqrt((mn - 1) / mn)

  set.seed(6)
  left <- contCoef(tab, conf.level = 0.95, sides = "left")
  set.seed(6)
  right <- contCoef(tab, conf.level = 0.95, sides = "right")

  expect_equal(unname(left[["uci"]]), cMax)
  expect_lt(unname(left[["lci"]]), unname(left[["est"]]))

  expect_equal(unname(right[["lci"]]), 0)
  expect_gt(unname(right[["uci"]]), unname(right[["est"]]))
})


test_that("cramerV keeps estimate and interval on the same scale", {

  tab <- as.table(rbind(c(26, 26, 23, 18,  9),
                        c( 6,  7,  9, 14, 23)))

  plain <- cramerV(tab, conf.level = 0.95)
  corr  <- cramerV(tab, conf.level = 0.95, correct = TRUE)

  # the corrected estimate is smaller, and so must its bounds be
  expect_lt(unname(corr[["est"]]), unname(plain[["est"]]))
  expect_lt(unname(corr[["lci"]]), unname(plain[["lci"]]))
  expect_lt(unname(corr[["uci"]]), unname(plain[["uci"]]))

  # and the interval still brackets its own estimate
  expect_lte(unname(corr[["lci"]]), unname(corr[["est"]]))
  expect_gte(unname(corr[["uci"]]), unname(corr[["est"]]))
})


test_that("cramerV rejects a misspelled method even without a CI", {
  tab <- table(c("a", "a", "b", "b"), c("x", "y", "x", "y"))
  expect_error(cramerV(tab, method = "nochisq"), "arg")
})


test_that("cStat returns an unnamed scalar and refuses a constant response", {

  set.seed(7)
  x <- runif(100)
  y <- rbinom(100, 1, 0.5)

  est <- cStat(x, resp = y)
  expect_null(names(est))
  expect_true(est >= 0 && est <= 1)

  expect_error(cStat(x, resp = rep(1, 100)), "both outcome classes")
})


test_that("cStat reports the same estimate with and without an interval", {

  set.seed(8)
  x <- runif(200)
  y <- rbinom(200, 1, plogis(2 * x - 1))

  plain <- cStat(x, resp = y)
  withCi <- cStat(x, resp = y, conf.level = 0.95)

  expect_equal(unname(withCi[["est"]]), unname(plain))
})


test_that("countWorkDays counts both endpoints and handles reversed dates", {

  # 2019-01-07 is a Monday
  mon <- as.Date("2019-01-07")
  expect_equal(countWorkDays(mon, mon), 1L)
  expect_equal(countWorkDays(mon, mon + 4), 5L)      # Mon..Fri
  expect_equal(countWorkDays(mon, mon + 6), 5L)      # full week
  expect_equal(countWorkDays(as.Date("2019-01-05"),
                             as.Date("2019-01-05")), 0L)  # Saturday

  # reversed pair used to abort with "wrong sign in 'by' argument"
  expect_equal(countWorkDays(mon + 4, mon), 0L)
})


test_that("countWorkDays subtracts holidays once and validates nonworkdays", {

  mon <- as.Date("2019-01-07")

  expect_equal(countWorkDays(mon, mon + 4,
                             holiday = c("2019-01-08", "2019-01-09")), 3L)
  # duplicated holidays count once
  expect_equal(countWorkDays(mon, mon + 4,
                             holiday = rep("2019-01-08", 3)), 4L)
  # a holiday on a weekend changes nothing
  expect_equal(countWorkDays(mon, mon + 6, holiday = "2019-01-12"), 5L)

  expect_error(countWorkDays(mon, mon + 4, nonworkdays = "Sunday"), "subset")
})


test_that("cronbachAlpha keeps its shape for missing input", {

  set.seed(9)
  d <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10))
  d$a[1] <- NA

  res <- cronbachAlpha(d, conf.level = 0.95)
  expect_named(res, c("est", "lci", "uci"))
  expect_true(all(is.na(res)))

  resCond <- cronbachAlpha(d, returnConditional = TRUE, conf.level = 0.95)
  expect_named(resCond, c("unconditional", "conditional"))
})


test_that("cronbachAlpha closes the bounded side at 1", {

  set.seed(10)
  d <- as.data.frame(matrix(rnorm(60), ncol = 3))

  left <- cronbachAlpha(d, conf.level = 0.95, sides = "left")
  right <- cronbachAlpha(d, conf.level = 0.95, sides = "right")

  expect_equal(unname(left[["uci"]]), 1)      # alpha <= 1
  expect_identical(unname(right[["lci"]]), -Inf)  # unbounded below
})
