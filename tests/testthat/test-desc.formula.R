
# desc.formula() -- formula interface of desc() ------------------------------

# 24 rows: every group keeps at least 4 observations after the subsets used
# below. With 2 per group the absolute deviations from the group median are
# equal, and leveneTest() in .descNQ() warns about an "essentially perfect
# fit".
d.desc <- data.frame(
  y = c(5.2, 3.7, 4.7, 5.5, 5.6, 1.7, 5.0, 2.8, 3.4, 4.0, 3.3, 6.2,
        5.3, 7.4, 5.2, 5.0, 4.3, 2.7, 5.6, 3.7, 4.4, 4.7, 6.7, 5.5),
  x = c(1.9, 2.2, 2.1, 3.1, 2.7, 0.5, 3.1, 0.5, 1.7, 1.8, 1.5, 3.3,
        2.9, 4.1, 2.2, 2.2, 1.6, 1.9, 2.3, 1.4, 2.3, 1.8, 2.9, 2.5),
  g = factor(rep(c("a", "b", "c"), times = 8)),
  h = factor(rep(c("s", "t"), each = 12)),
  f = factor(rep(c("lo", "hi"), times = 12))
)


test_that("desc(y ~ g) describes a numeric response by a factor", {
  r <- desc(y ~ g, data = d.desc)
  expect_s3_class(r, "Desc")
  expect_named(r, "g")
  expect_s3_class(r$g, "Desc.nq")
  expect_equal(r$g$data$y, d.desc$y)
  expect_equal(r$g$data$x, d.desc$g)
  expect_identical(r$g$meta$main, "y ~ g (d.desc)")
})


test_that("desc(y ~ x) describes a numeric pair", {
  # the predictor, not the (empty) grouping field: reading rf$group for a
  # numeric-numeric design degraded to "Unknown type combination: n"
  r <- desc(y ~ x, data = d.desc)
  expect_s3_class(r$x, "Desc.nn")
  expect_equal(r$x$data$x, d.desc$x)
})


test_that("desc(f ~ g) describes a pair of factors", {
  r <- desc(f ~ g, data = d.desc)
  expect_s3_class(r$g, "Desc.qq")
})


test_that("several right-hand side terms give one description each", {
  r <- desc(y ~ g + x + h, data = d.desc)
  expect_named(r, c("g", "x", "h"))
  expect_s3_class(r$g, "Desc.nq")
  expect_s3_class(r$x, "Desc.nn")
  expect_s3_class(r$h, "Desc.nq")
})


test_that("desc.formula works without data", {
  # 'data' was forced by do.call(list(data = data)) and failed with
  # "argument "data" is missing"
  yy <- d.desc$y
  gg <- d.desc$g
  r <- desc(yy ~ gg)
  expect_s3_class(r$gg, "Desc.nq")
  expect_equal(r$gg$data$y, yy)
})


test_that("desc(y ~ 1) is a one-sample description titled by the response", {
  # y ~ 1 has no term label and used to return an empty result
  r <- desc(y ~ 1, data = d.desc)
  expect_s3_class(r, "Desc")
  expect_named(r, "1")
  expect_identical(r[[1L]]$meta$main, "y")
})


test_that("a subset shows up in the one-sample title", {
  r <- desc(y ~ 1, data = d.desc, subset = h == "s")
  expect_identical(r[[1L]]$meta$main, "y[h == \"s\"]")
  # an explicit main wins
  r2 <- desc(y ~ 1, data = d.desc, subset = h == "s", main = "Titel")
  expect_identical(r2[[1L]]$meta$main, "Titel")
})


test_that("subset is evaluated in data", {
  r <- desc(y ~ g, data = d.desc, subset = h == "s")
  expect_equal(r$g$data$y, d.desc$y[d.desc$h == "s"])
  expect_equal(as.character(r$g$data$x),
               as.character(d.desc$g[d.desc$h == "s"]))

  # the same subset applies to every term
  r2 <- desc(y ~ g + x, data = d.desc, subset = x > 2)
  expect_equal(r2$g$data$y, d.desc$y[d.desc$x > 2])
  expect_equal(r2$x$data$x, d.desc$x[d.desc$x > 2])
})


test_that("subset finds variables of a calling function", {
  f <- function(dat, lim) desc(y ~ g, data = dat, subset = x > lim)
  r <- f(d.desc, 2)
  expect_equal(r$g$data$y, d.desc$y[d.desc$x > 2])
})


test_that("y ~ a:b describes the response by the cells", {
  r <- desc(y ~ g:h, data = d.desc)
  expect_named(r, "g:h")
  expect_s3_class(r[["g:h"]], "Desc.nq")
  expect_equal(nlevels(droplevels(r[["g:h"]]$data$x)), 6L)
  expect_true(all(grepl(":", levels(r[["g:h"]]$data$x), fixed = TRUE)))
})


test_that("y ~ a + b is not read as cells", {
  r <- desc(y ~ g + h, data = d.desc)
  expect_named(r, c("g", "h"))
  expect_equal(r$h$data$x, d.desc$h)
})


test_that("missing values are passed on and counted in the pair summary", {
  dn <- d.desc
  dn$y[c(2, 5)] <- NA
  dn$g[7] <- NA
  r <- desc(y ~ g, data = dn)
  expect_equal(r$g$pair$nTotal, nrow(dn))
  expect_equal(r$g$pair$nMissing, 3L)
  expect_equal(r$g$pair$nMissingGroups, 1L)
  expect_equal(r$g$pair$nGroups, 3L)

  # na.omit removes them before the description
  r2 <- desc(y ~ g, data = dn, na.action = na.omit)
  expect_equal(r2$g$pair$nTotal, nrow(dn) - 3L)
  expect_equal(r2$g$pair$nMissing, 0L)
})


test_that("desc.formula runs without warnings on well-conditioned data", {
  # guards the data above: the subsets must not leave degenerate groups
  expect_no_warning(desc(y ~ g, data = d.desc, subset = h == "s"))
  expect_no_warning(desc(y ~ g, data = d.desc, subset = x > 2))
  expect_no_warning(desc(y ~ g:h, data = d.desc))
})


test_that("desc.formula rejects designs it cannot describe", {
  expect_error(desc(y ~ g | h, data = d.desc))
  expect_error(desc(y ~ 1, data = d.desc, subset = quote(h == "s")))
})
