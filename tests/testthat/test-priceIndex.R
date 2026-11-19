
p0 <- c(2.0, 5.0, 10.0)
p1 <- c(2.2, 5.5, 9.0)
q0 <- c(100, 40, 10)
q1 <- c(90, 45, 14)

estimators <- eval(formals(priceIndex)$estimator)
unweighted <- c("jevons", "carli", "dutot")


test_that("hand-computed reference values", {

  expect_equal(priceIndex(p0, p1, q0), 530 / 500)
  expect_equal(priceIndex(p0, p1, q0, q1, estimator = "paasche"),
               571.5 / 545)
  expect_equal(priceIndex(p0, p1, q0, q1, estimator = "fisher"),
               sqrt(530 / 500 * 571.5 / 545))
  expect_equal(priceIndex(p0, p1, q0, q1, estimator = "drobisch"),
               (530 / 500 + 571.5 / 545) / 2)
  expect_equal(priceIndex(p0, p1, q0, q1, estimator = "marshall-edgeworth"),
               sum(p1 * (q0 + q1)) / sum(p0 * (q0 + q1)))
  expect_equal(priceIndex(p0, p1, q0, q1, estimator = "walsh"),
               sum(p1 * sqrt(q0 * q1)) / sum(p0 * sqrt(q0 * q1)))

  s0 <- p0 * q0 / sum(p0 * q0)
  s1 <- p1 * q1 / sum(p1 * q1)
  r <- p1 / p0
  expect_equal(priceIndex(p0, p1, q0, q1, estimator = "tornqvist"),
               prod(r^((s0 + s1) / 2)))
  expect_equal(priceIndex(p0, p1, q0, estimator = "geo-laspeyres"),
               prod(r^s0))
  expect_equal(priceIndex(p0, p1, q1 = q1, estimator = "geo-paasche"),
               prod(r^s1))
  expect_equal(priceIndex(p0, p1, q1 = q1, estimator = "palgrave"),
               sum(s1 * r))

  expect_equal(priceIndex(p0, p1, estimator = "jevons"), prod(r)^(1 / 3))
  expect_equal(priceIndex(p0, p1, estimator = "carli"), mean(r))
  expect_equal(priceIndex(p0, p1, estimator = "dutot"), sum(p1) / sum(p0))

})


test_that("identity and proportionality hold for every estimator", {

  for (e in estimators) {
    expect_equal(priceIndex(p0, p0, q0, q1, estimator = e), 1, info = e)
    expect_equal(priceIndex(p0, 1.3 * p0, q0, q1, estimator = e), 1.3,
                 info = e)
  }

})


test_that("symmetric estimators satisfy time reversal", {

  for (e in c("fisher", "tornqvist", "walsh", "marshall-edgeworth",
              "sato-vartia", "jevons", "dutot")) {
    expect_equal(priceIndex(p0, p1, q0, q1, estimator = e) *
                   priceIndex(p1, p0, q1, q0, estimator = e), 1, info = e)
  }

})


test_that("Fisher and Sato-Vartia satisfy factor reversal", {

  for (e in c("fisher", "sato-vartia")) {
    expect_equal(priceIndex(p0, p1, q0, q1, estimator = e) *
                   quantityIndex(q0, q1, p0, p1, estimator = e),
                 sum(p1 * q1) / sum(p0 * q0), info = e)
  }
  # Laspeyres price x Paasche quantity decomposes the value ratio as well
  expect_equal(priceIndex(p0, p1, q0) *
                 quantityIndex(q0, q1, p0, p1, estimator = "paasche"),
               sum(p1 * q1) / sum(p0 * q0))

})


test_that("the index does not depend on the scale of the quantities", {

  for (e in setdiff(estimators, unweighted)) {
    expect_equal(priceIndex(p0, p1, 7 * q0, 7 * q1, estimator = e),
                 priceIndex(p0, p1, q0, q1, estimator = e), info = e)
  }
  # period-specific scales cancel as well, except for Marshall-Edgeworth
  for (e in setdiff(estimators, c(unweighted, "marshall-edgeworth"))) {
    expect_equal(priceIndex(p0, p1, 7 * q0, 0.2 * q1, estimator = e),
                 priceIndex(p0, p1, q0, q1, estimator = e), info = e)
  }

})


test_that("result is an unnamed scalar", {

  res <- priceIndex(c(a = 1, b = 2), c(a = 2, b = 3), c(a = 1, b = 1))
  expect_length(res, 1L)
  expect_null(names(res))

})


test_that("zero quantities are legitimate", {

  expect_equal(priceIndex(p0, p1, c(100, 0, 10)),
               sum(p1[-2] * c(100, 10)) / sum(p0[-2] * c(100, 10)))
  # an item absent in one period gets weight zero in Sato-Vartia
  s0 <- p0 * c(100, 0, 10) / sum(p0 * c(100, 0, 10))
  s1 <- p1 * q1 / sum(p1 * q1)
  w <- c((s1[1] - s0[1]) / log(s1[1] / s0[1]), 0,
         (s1[3] - s0[3]) / log(s1[3] / s0[3]))
  expect_equal(priceIndex(p0, p1, c(100, 0, 10), q1,
                          estimator = "sato-vartia"),
               exp(sum(w * log(p1 / p0)) / sum(w)))
  # basket without any expenditure: not defined
  expect_identical(priceIndex(p0, p1, c(0, 0, 0)), NA_real_)

})


test_that("NA rule", {

  p1na <- c(2.2, NA, 9.0)
  expect_identical(priceIndex(p0, p1na, q0), NA_real_)
  expect_equal(priceIndex(p0, p1na, q0, na.rm = TRUE),
               priceIndex(p0[-2], p1[-2], q0[-2]))
  expect_identical(priceIndex(p0, c(2.2, NaN, 9.0), q0), NA_real_)

  # a missing value in a quantity the estimator does not use is irrelevant
  expect_equal(priceIndex(p0, p1, q0, c(NA, 1, 1)), priceIndex(p0, p1, q0))
  # ... but it counts where it is used, item-wise
  expect_identical(priceIndex(p0, p1, q0, c(NA, 45, 14),
                              estimator = "fisher"), NA_real_)
  expect_equal(priceIndex(p0, p1, q0, c(NA, 45, 14), estimator = "fisher",
                          na.rm = TRUE),
               priceIndex(p0[-1], p1[-1], q0[-1], q1[-1],
                          estimator = "fisher"))

  # nothing left
  expect_identical(priceIndex(NA_real_, 1, 1, na.rm = TRUE), NA_real_)
  expect_identical(priceIndex(numeric(0), numeric(0), numeric(0)), NA_real_)

  # invalid arguments are reported before any NA is returned
  expect_error(priceIndex(c(-1, NA), c(1, 1), c(1, 1)), "p0")
  expect_error(priceIndex(p0, p1na, q0, na.rm = NA), "na.rm")

})


test_that("invalid arguments are errors", {

  expect_error(priceIndex(p0, p1[-1], q0), "p0")
  expect_error(priceIndex(letters[1:3], p1, q0), "p0")
  expect_error(priceIndex(p0, p1), "q0")
  expect_error(priceIndex(p0, p1, q0, estimator = "fisher"), "q1")
  expect_error(priceIndex(p0, p1, q1 = q1, estimator = "fisher"), "q0")
  expect_error(priceIndex(p0, p1, q0[-1]), "q0")
  expect_error(priceIndex(p0, p1, -q0), "q0")
  expect_error(priceIndex(p0, c(0, 1, 1), q0, q1, estimator = "tornqvist"),
               "p1")
  expect_error(priceIndex(p0, c(Inf, 1, 1), q0), "p1")
  expect_error(priceIndex(p0, p1, q0, estimator = "nonsense"))
  expect_error(quantityIndex(q0, q1), "p0")

  # a zero price is fine for the fixed-basket indices
  expect_equal(priceIndex(p0, c(0, 5.5, 9), q0), (220 + 90) / 500)

})


# -- chainedIndex --------------------------------------------------------------

d <- data.frame(
  year = c(2020, 2020, 2020, 2021, 2021, 2021, 2021, 2022, 2022, 2022),
  id   = c("a", "b", "c", "a", "b", "c", "d", "a", "b", "d"),
  p    = c(2.0, 5.0, 10.0, 2.2, 5.5, 9.0, 20.0, 2.1, 5.6, 16.0),
  q    = c(100, 40, 10, 90, 45, 14, 5, 95, 40, 12)
)


test_that("chained index multiplies the links over matched items", {

  res <- chainedIndex(d$p, d$id, d$year, d$q, estimator = "tornqvist")

  expect_s3_class(res, "data.frame")
  expect_named(res, c("period", "index", "link", "n"))
  expect_equal(res$period, 2020:2022)
  expect_equal(res$n, c(NA, 3L, 3L))

  l1 <- priceIndex(p0, p1, q0, q1, estimator = "tornqvist")
  l2 <- priceIndex(c(2.2, 5.5, 20), c(2.1, 5.6, 16),
                   c(90, 45, 5), c(95, 40, 12), estimator = "tornqvist")
  expect_equal(res$link, c(NA, l1, l2))
  expect_equal(res$index, c(1, l1, l1 * l2))

})


test_that("row order does not matter", {

  set.seed(1)
  i <- sample(nrow(d))
  expect_equal(chainedIndex(d$p[i], d$id[i], d$year[i], d$q[i]),
               chainedIndex(d$p, d$id, d$year, d$q))

})


test_that("rebasing rescales the index and keeps the links", {

  a <- chainedIndex(d$p, d$id, d$year, estimator = "jevons")
  b <- chainedIndex(d$p, d$id, d$year, estimator = "jevons", base = 2022)

  expect_equal(b$index, a$index / a$index[3])
  expect_equal(b$link, a$link)
  expect_error(chainedIndex(d$p, d$id, d$year, d$q, base = 1999), "base")
  expect_error(chainedIndex(d$p, d$id, d$year, d$q, base = 2020:2021),
               "base")

})


test_that("period may be a factor or a Date", {

  f <- factor(d$year, levels = c(2022, 2021, 2020))
  expect_equal(as.character(chainedIndex(d$p, d$id, f, d$q)$period),
               c("2022", "2021", "2020"))

  dt <- as.Date(paste0(d$year, "-01-01"))
  expect_equal(chainedIndex(d$p, d$id, dt, d$q)$index,
               chainedIndex(d$p, d$id, d$year, d$q)$index)

})


test_that("a missing value only affects the periods beyond its link", {

  p <- d$p
  p[9] <- NA   # item b in 2022

  res <- chainedIndex(p, d$id, d$year, d$q)
  expect_equal(is.na(res$index), c(FALSE, FALSE, TRUE))

  res <- chainedIndex(p, d$id, d$year, d$q, base = 2022)
  expect_equal(is.na(res$index), c(TRUE, TRUE, FALSE))

  res <- chainedIndex(p, d$id, d$year, d$q, na.rm = TRUE)
  expect_false(anyNA(res$index))
  expect_equal(res$n, c(NA, 3L, 3L))

})


test_that("a break in the panel gives NA with a warning", {

  expect_warning(
    res <- chainedIndex(c(1, 2, 3, 4), c("a", "b", "a", "b"),
                        c(1, 2, 3, 4), estimator = "jevons"),
    "no matched items")
  expect_equal(res$n, c(NA, 0L, 0L, 0L))
  expect_true(all(is.na(res$index[-1])))

})


test_that("single period and invalid input", {

  res <- chainedIndex(c(1, 2), c("a", "b"), c(1, 1), c(1, 1))
  expect_equal(res$index, 1)
  expect_equal(nrow(res), 1L)

  expect_error(chainedIndex(d$p, d$id[-1], d$year, d$q), "id")
  expect_error(chainedIndex(d$p, replace(d$id, 2, NA), d$year, d$q), "id")
  expect_error(chainedIndex(d$p, replace(d$id, 2, "a"), d$year, d$q),
               "unique")
  expect_error(chainedIndex(d$p, d$id, d$year), "'q'")
  expect_error(chainedIndex(d$p, d$id, d$year, d$q[-1]), "'q'")
  expect_error(chainedIndex(-d$p, d$id, d$year, d$q), "'p'")

})
