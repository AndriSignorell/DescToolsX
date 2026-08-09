tab <- as.table(rbind(
  c(26, 26, 23, 18,  9),
  c( 6,  7,  9, 14, 23)
))

# the six measures and the range each of them lives in
rng <- list(gamma  = c(-1, 1), tauA = c(-1, 1), tauB = c(-1, 1),
            tauC   = c(-1, 1), somers = c(-1, 1), cstat = c(0, 1))


test_that("ordAssocs() applies the range of each measure, not one range for all", {

  two   <- ordAssocs(tab, conf.level = 0.95)
  left  <- ordAssocs(tab, conf.level = 0.95, sides = "left")
  right <- ordAssocs(tab, conf.level = 0.95, sides = "right")

  for (m in names(rng)) {

    lo <- rng[[m]][1L]
    hi <- rng[[m]][2L]

    # the estimate never depends on the sidedness
    expect_equal(unname(two[[m]]["est"]), unname(left[[m]]["est"]), info = m)
    expect_equal(unname(two[[m]]["est"]), unname(right[[m]]["est"]), info = m)

    # this is the part a single shared range would get wrong: cstat must
    # open at 0/1, the other five at -1/1
    expect_equal(unname(left[[m]]["uci"]),  hi, info = m)
    expect_equal(unname(right[[m]]["lci"]), lo, info = m)

    # and the two-sided interval stays inside the range
    expect_gte(unname(two[[m]]["lci"]), lo)
    expect_lte(unname(two[[m]]["uci"]), hi)
  }
})


test_that("the one-sided bound equals the two-sided one at the adjusted level", {

  left  <- ordAssocs(tab, conf.level = 0.95, sides = "left")
  right <- ordAssocs(tab, conf.level = 0.95, sides = "right")
  two   <- ordAssocs(tab, conf.level = 0.90)

  for (m in names(rng)) {
    expect_equal(unname(left[[m]]["lci"]),  unname(two[[m]]["lci"]),  info = m)
    expect_equal(unname(right[[m]]["uci"]), unname(two[[m]]["uci"]), info = m)
  }
})


test_that("every extractor carries sides through", {

  calls <- list(
    gamma  = function(...) gkGamma(tab, ...),
    tauA   = function(...) kendallTauA(tab, ...),
    tauB   = function(...) kendallTauB(tab, ...),
    tauC   = function(...) stuartTauC(tab, ...),
    somers = function(...) somersDelta(tab, ...)
  )

  for (m in names(calls)) {

    f  <- calls[[m]]
    lo <- rng[[m]][1L]
    hi <- rng[[m]][2L]

    two   <- f(conf.level = 0.95)
    left  <- f(conf.level = 0.95, sides = "left")
    right <- f(conf.level = 0.95, sides = "right")

    expect_named(left, c("est", "lci", "uci"), info = m)

    expect_equal(unname(left[["est"]]), unname(two[["est"]]), info = m)
    expect_equal(left[["uci"]],  hi, info = m)
    expect_equal(right[["lci"]], lo, info = m)

    # a one-sided bound carries the whole alpha, so it is tighter
    expect_gte(left[["lci"]],  two[["lci"]])
    expect_lte(right[["uci"]], two[["uci"]])

    # and it agrees with the extractor's parent
    expect_equal(left[["lci"]],
                 unname(ordAssocs(tab, which = m, conf.level = 0.95,
                                  sides = "left")[[m]]["lci"]), info = m)
  }
})


test_that("sides is matched even when no interval is requested", {

  # the check must not hide inside the interval branch
  expect_error(ordAssocs(tab, sides = "links"), "two.sided")
  expect_error(gkGamma(tab, sides = "links"), "two.sided")
  expect_error(kendallTauA(tab, sides = "links"), "two.sided")
  expect_error(kendallTauB(tab, sides = "links"), "two.sided")
  expect_error(stuartTauC(tab, sides = "links"), "two.sided")
  expect_error(somersDelta(tab, sides = "links"), "two.sided")

  # and the point estimate is untouched by a valid sides
  expect_equal(kendallTauB(tab), kendallTauB(tab, sides = "left"))
})


test_that("a one-sided interval below conf.level 0.5 is refused", {

  expect_error(ordAssocs(tab, conf.level = 0.4, sides = "left"), "0.5")
  expect_error(gkGamma(tab, conf.level = 0.5, sides = "right"), "0.5")
  expect_error(somersDelta(tab, conf.level = 0.2, sides = "left"), "0.5")

  # two-sided is unaffected, and NA never reaches the guard
  expect_silent(ordAssocs(tab, conf.level = 0.4))
  expect_silent(gkGamma(tab, sides = "left"))
})


test_that("conf.level is validated once, in the parent", {

  # stuartTauC and somersDelta used to carry their own copies, which said
  # "a single value" where the shared check says "a single number"
  for (f in list(ordAssocs, gkGamma, kendallTauA, kendallTauB,
                 stuartTauC, somersDelta)) {
    expect_error(f(tab, conf.level = c(0.9, 0.95)), "conf.level")
    expect_error(f(tab, conf.level = NULL), "conf.level")
    expect_error(f(tab, conf.level = NaN), "conf.level")
    expect_error(f(tab, conf.level = 0), "conf.level")
    expect_error(f(tab, conf.level = 1), "conf.level")
  }
})


test_that("sides works in xy mode as well", {

  x <- c(1, 2, 2, 3, 3, 3, 4, 5)
  y <- c(1, 3, 2, 1, 5, 3, 4, 5)

  two  <- kendallTauA(x, y, conf.level = 0.95)
  left <- kendallTauA(x, y, conf.level = 0.95, sides = "left")

  expect_equal(unname(left[["est"]]), unname(two[["est"]]))
  expect_equal(left[["uci"]], 1)
  expect_gte(left[["lci"]], two[["lci"]])

  expect_equal(left[["lci"]],
               kendallTauA(x, y, conf.level = 0.90)[["lci"]])
})


test_that("a measure without a recorded range is refused, not guessed", {

  # .ordAssocRange is the single place where a new measure has to be
  # entered; forgetting it must not silently borrow somebody else's bounds
  expect_error(
    DescToolsX:::.ordAssocApplySides(list(newThing = c(0.5, 0.4, 0.6)),
                                     "left"),
    "range")
})
