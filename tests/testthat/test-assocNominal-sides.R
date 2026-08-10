tab <- as.table(rbind(c(26, 26, 23, 18, 9),
                      c( 6,  7,  9, 14, 23)))

tab22 <- as.table(rbind(c(30, 12),
                        c(14, 25)))


# one entry per function: the call, and the range the measure lives in
cases <- list(
  gkTau_row    = list(f = function(...) gkTau(tab, direction = "row", ...),
                      lo = 0,  hi = 1),
  gkTau_col    = list(f = function(...) gkTau(tab, direction = "column", ...),
                      lo = 0,  hi = 1),
  uncertCoef   = list(f = function(...) uncertCoef(tab, ...),
                      lo = 0,  hi = 1),
  uncertCoefR  = list(f = function(...) uncertCoef(tab, direction = "row", ...),
                      lo = 0,  hi = 1),
  yuleQ        = list(f = function(...) yuleQ(tab22, ...),
                      lo = -1, hi = 1),
  yuleY        = list(f = function(...) yuleY(tab22, ...),
                      lo = -1, hi = 1),
  relRiskScore = list(f = function(...) relRisk(tab22, method = "score", ...),
                      lo = 0,  hi = Inf),
  relRiskWald  = list(f = function(...) relRisk(tab22, method = "wald", ...),
                      lo = 0,  hi = Inf)
)


test_that("the open side is closed at the measure's own range", {

  for (nm in names(cases)) {

    f  <- cases[[nm]]$f
    lo <- cases[[nm]]$lo
    hi <- cases[[nm]]$hi

    two   <- f(conf.level = 0.95)
    left  <- f(conf.level = 0.95, sides = "left")
    right <- f(conf.level = 0.95, sides = "right")

    expect_named(left, c("est", "lci", "uci"), info = nm)

    # the estimate never depends on the sidedness
    expect_equal(unname(left[["est"]]),  unname(two[["est"]]), info = nm)
    expect_equal(unname(right[["est"]]), unname(two[["est"]]), info = nm)

    # this is the part a single shared range would get wrong: relRisk
    # opens to Inf, yule to +/-1, the two nominal measures to 0/1
    expect_equal(left[["uci"]],  hi, info = nm)
    expect_equal(right[["lci"]], lo, info = nm)

    # and the two-sided interval stays inside the range
    expect_gte(two[["lci"]], lo)
    expect_lte(two[["uci"]], hi)
  }
})


test_that("the one-sided bound equals the two-sided one at the adjusted level", {

  for (nm in names(cases)) {

    f <- cases[[nm]]$f

    left  <- f(conf.level = 0.95, sides = "left")
    right <- f(conf.level = 0.95, sides = "right")
    two   <- f(conf.level = 0.90)

    expect_equal(left[["lci"]],  two[["lci"]], info = nm)
    expect_equal(right[["uci"]], two[["uci"]], info = nm)

    # a one-sided bound carries the whole alpha, so it is tighter
    expect_gte(left[["lci"]],  f(conf.level = 0.95)[["lci"]])
    expect_lte(right[["uci"]], f(conf.level = 0.95)[["uci"]])
  }
})


test_that("a one-sided interval below conf.level 0.5 is refused", {

  for (nm in names(cases)) {
    f <- cases[[nm]]$f
    expect_error(f(conf.level = 0.4, sides = "left"),  "0.5", info = nm)
    expect_error(f(conf.level = 0.5, sides = "right"), "0.5", info = nm)
    expect_silent(f(conf.level = 0.4))
  }
})


test_that("sides is matched even when no interval is requested", {

  expect_error(gkTau(tab, sides = "links"), "two.sided")
  expect_error(uncertCoef(tab, sides = "links"), "two.sided")
  expect_error(relRisk(tab22, sides = "links"), "two.sided")
  expect_error(yuleQ(tab22, sides = "links"), "two.sided")

  # gkTau matched 'direction' only inside the branch that used it
  expect_error(gkTau(tab, direction = "diagonal"), "row")
})


test_that("conf.level is validated through the shared helper", {

  for (f in list(function(...) gkTau(tab, ...),
                 function(...) uncertCoef(tab, ...),
                 function(...) relRisk(tab22, ...),
                 function(...) yuleQ(tab22, ...))) {
    expect_error(f(conf.level = c(0.9, 0.95)), "conf.level")
    expect_error(f(conf.level = NULL), "conf.level")
    expect_error(f(conf.level = NaN), "conf.level")
    expect_error(f(conf.level = 0), "conf.level")
    expect_error(f(conf.level = 1), "conf.level")
  }

  # .yuleCoef checked conf.level only AFTER the early return for
  # conf.level = NA, so a length-2 value aborted inside that if()
  expect_error(yuleY(tab22, conf.level = c(0.9, 0.95)), "conf.level")
})


test_that("a zero cell still gives the limiting Yule bounds", {

  # logOR is infinite here; the explicit limits must survive applySides()
  z <- as.table(rbind(c(10, 0), c(5, 8)))

  q <- yuleQ(z, conf.level = 0.95)
  expect_equal(unname(q[["est"]]), 1)
  expect_equal(q[["uci"]], 1)
  expect_gte(q[["lci"]], -1)

  # and one-sided does not turn the limit into something else
  expect_equal(yuleQ(z, conf.level = 0.95, sides = "right")[["lci"]], -1)
})


test_that("relRisk keeps its infinite upper bound where it belongs", {

  # x2 == 0 makes the estimate infinite; the interval must not be clamped
  # to a finite number by the sides machinery
  z <- as.table(rbind(c(8, 2), c(0, 10)))

  r <- relRisk(z, conf.level = 0.95, method = "score")
  expect_true(is.infinite(r[["est"]]) || r[["est"]] > 0)
  expect_gte(r[["lci"]], 0)

  expect_equal(relRisk(z, conf.level = 0.95, sides = "left")[["uci"]], Inf)
})


test_that("randolphKappa still refuses an interval rather than faking one", {

  # deliberately NOT given a sides argument: the function has no interval,
  # so a sides formal would be the ninth documented-argument-without-effect
  m <- matrix(c(1, 2, 1, 1, 2, 2, 1, 1, 2), nrow = 3)

  expect_length(randolphKappa(m), 1L)
  expect_error(randolphKappa(m, conf.level = 0.95), "not implemented")
  expect_false("sides" %in% names(formals(randolphKappa)))
})
