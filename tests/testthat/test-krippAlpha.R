df <- data.frame(
  r1 = c(1, 2, 3, 3, 2, 1, 4, 1, 2, NA),
  r2 = c(1, 2, 3, 3, 2, 2, 4, 1, 2, 5),
  r3 = c(NA, 3, 3, 3, 2, 3, 4, 2, 2, 5),
  r4 = c(1, 2, 3, 3, 2, 4, 4, 1, 2, 5)
)


test_that("krippAlpha() runs at all", {

  # regression: the formal was renamed to 'type' while the body and the
  # documentation kept using 'method', so match.arg(method) could not find
  # its argument and every call failed with "object 'method' not found"
  expect_silent(a <- krippAlpha(df))
  expect_length(a, 1L)
  expect_true(is.numeric(a))

  # 'metric' selects the difference function, i.e. WHICH alpha is
  # computed. It is not called 'method', because method means the interval
  # method everywhere else in the suite - and 'type' is already taken by
  # the bootstrap interval type that travels through ...
  expect_true("metric" %in% names(formals(krippAlpha)))
  expect_false(any(c("method", "type") %in% names(formals(krippAlpha))))
  expect_true("output" %in% names(formals(krippAlpha)))
  expect_false("out" %in% names(formals(krippAlpha)))

  for (m in c("nominal", "ordinal", "interval", "ratio"))
    expect_silent(krippAlpha(df, metric = m))
})


test_that("conf.level is honoured, not silently replaced", {

  # regression: conf.level was never passed to bootCI(), so every request
  # came back at bootCI's own default level
  set.seed(1); narrow <- krippAlpha(df, conf.level = 0.80, R = 199)
  set.seed(1); wide   <- krippAlpha(df, conf.level = 0.99, R = 199)

  expect_named(narrow, c("est", "lci", "uci"))
  expect_equal(narrow[["est"]], wide[["est"]])

  expect_gt(narrow[["lci"]], wide[["lci"]])
  expect_lt(narrow[["uci"]], wide[["uci"]])
})


test_that("conf.level is validated through the shared helper", {

  expect_error(krippAlpha(df, conf.level = c(0.9, 0.95)), "conf.level")
  expect_error(krippAlpha(df, conf.level = NULL), "conf.level")
  expect_error(krippAlpha(df, conf.level = NaN), "conf.level")
  expect_error(krippAlpha(df, conf.level = 0), "conf.level")
  expect_error(krippAlpha(df, conf.level = 1), "conf.level")
})


test_that("sides closes the open side at alpha's own range", {

  set.seed(2); two   <- krippAlpha(df, conf.level = 0.95, R = 199)
  set.seed(2); left  <- krippAlpha(df, conf.level = 0.95, R = 199, sides = "left")
  set.seed(2); right <- krippAlpha(df, conf.level = 0.95, R = 199, sides = "right")

  # alpha lies in [-1, 1]; Inf would claim a value it cannot take
  expect_equal(left[["uci"]], 1)
  expect_equal(right[["lci"]], -1)

  expect_equal(left[["est"]], two[["est"]])
  expect_gte(left[["lci"]],  two[["lci"]])
  expect_lte(right[["uci"]], two[["uci"]])

  # left(gamma) reads the same end as two.sided(2*gamma - 1)
  set.seed(3); l  <- krippAlpha(df, conf.level = 0.95, R = 199, sides = "left")
  set.seed(3); t9 <- krippAlpha(df, conf.level = 0.90, R = 199)
  expect_equal(l[["lci"]], t9[["lci"]])
})


test_that("sides is matched even when no interval is requested", {

  expect_error(krippAlpha(df, sides = "links"), "two.sided")
  expect_error(krippAlpha(df, conf.level = 0.4, sides = "left"), "0.5")
  expect_silent(krippAlpha(df, conf.level = NA, sides = "left"))
})


test_that("output = 'ext' carries the same interval as output = 'def'", {

  set.seed(4); d <- krippAlpha(df, conf.level = 0.95, R = 199)
  set.seed(4); e <- krippAlpha(df, conf.level = 0.95, R = 199, output = "ext")

  expect_equal(e$ci, d)
  expect_equal(e$alpha, unname(d[["est"]]))

  # and without an interval the triple is still indexable
  f <- krippAlpha(df, output = "ext")
  expect_named(f$ci, c("est", "lci", "uci"))
  expect_true(all(is.na(f$ci)))

  # sides applies in the extended output too
  set.seed(4)
  g <- krippAlpha(df, conf.level = 0.95, R = 199, output = "ext",
                  sides = "left")
  expect_equal(g$ci[["uci"]], 1)
})
