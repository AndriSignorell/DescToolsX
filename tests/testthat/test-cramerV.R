tab <- table(Pizza$driver, Pizza$wine_delivered)


test_that("cramerV() point estimate is unchanged by the sides argument", {

  v <- cramerV(tab)

  for (s in c("two.sided", "left", "right"))
    expect_equal(unname(cramerV(tab, conf.level = 0.95, sides = s)["est"]), v)

  # and correct = TRUE still scales estimate and bounds together
  vc <- cramerV(tab, correct = TRUE)
  ci <- cramerV(tab, conf.level = 0.95, correct = TRUE)
  expect_equal(unname(ci["est"]), vc)
  expect_true(ci[["lci"]] <= vc && vc <= ci[["uci"]])
})


test_that("sides names the side carrying the finite bound", {

  for (m in c("ncchisq", "ncchisqadj", "fisher", "fisheradj")) {

    two   <- cramerV(tab, conf.level = 0.95, method = m)
    left  <- cramerV(tab, conf.level = 0.95, method = m, sides = "left")
    right <- cramerV(tab, conf.level = 0.95, method = m, sides = "right")

    # "left" is the analogue of alternative = "greater": finite lower
    # bound, the upper one opens to the top of the range
    expect_equal(left[["uci"]], 1, info = m)
    expect_equal(right[["lci"]], 0, info = m)

    # a one-sided bound carries the whole alpha, so it is tighter
    expect_gte(left[["lci"]],  two[["lci"]])
    expect_lte(right[["uci"]], two[["uci"]])
  }
})


test_that("the one-sided bound equals the two-sided one at the adjusted level", {

  # left(gamma) reads the same end as two.sided(2*gamma - 1); this is the
  # identity the level adjustment is built on, so it should hold exactly
  for (m in c("ncchisq", "ncchisqadj", "fisher", "fisheradj")) {

    left <- cramerV(tab, conf.level = 0.95, method = m, sides = "left")
    two  <- cramerV(tab, conf.level = 0.90, method = m)
    expect_equal(left[["lci"]], two[["lci"]], info = m)

    right <- cramerV(tab, conf.level = 0.95, method = m, sides = "right")
    expect_equal(right[["uci"]], two[["uci"]], info = m)
  }
})


test_that("bounds stay inside the attainable range", {

  for (m in c("ncchisq", "ncchisqadj", "fisher", "fisheradj")) {
    ci <- cramerV(tab, conf.level = 0.95, method = m)
    expect_true(ci[["lci"]] >= 0 && ci[["uci"]] <= 1, info = m)
    expect_true(ci[["lci"]] <= ci[["est"]] && ci[["est"]] <= ci[["uci"]],
                info = m)
  }
})


test_that("a one-sided interval below conf.level 0.5 is refused", {

  # the adjusted level would be non-positive; the Fisher half-width would
  # come out negative and reverse the bounds without a word
  expect_error(cramerV(tab, conf.level = 0.4, sides = "left"), "0.5")
  expect_error(cramerV(tab, conf.level = 0.5, sides = "right"), "0.5")

  # two-sided is unaffected
  expect_silent(cramerV(tab, conf.level = 0.4))
})


test_that("conf.level is validated before it is used", {

  expect_silent(cramerV(tab))
  expect_length(cramerV(tab, conf.level = NA), 1L)

  # these used to abort inside `&&`, with a message about the condition
  expect_error(cramerV(tab, conf.level = c(0.9, 0.95)), "conf.level")
  expect_error(cramerV(tab, conf.level = NULL), "conf.level")

  # NaN used to slip through and return the point estimate
  expect_error(cramerV(tab, conf.level = NaN), "conf.level")

  expect_error(cramerV(tab, conf.level = "0.95"), "conf.level")
  expect_error(cramerV(tab, conf.level = 0), "conf.level")
  expect_error(cramerV(tab, conf.level = 1), "conf.level")
})


test_that("sides and method are matched, not guessed", {
  
  # match.arg() nennt in der Meldung woertlich 'arg', nicht den
  # Argumentnamen - gepruft wird deshalb die Auswahlliste
  expect_error(cramerV(tab, conf.level = 0.95, sides = "links"),
               "two.sided")
  
  # "ncchi" ist zwischen ncchisq und ncchisqadj mehrdeutig
  expect_error(cramerV(tab, conf.level = 0.95, method = "ncchi"),
               "ncchisqadj")
  
  expect_equal(cramerV(tab, conf.level = 0.95, sides = "l"),
               cramerV(tab, conf.level = 0.95, sides = "left"))
})


test_that("... still reaches table()", {

  # regression from tranche c: the dots were accepted and dropped
  a <- cramerV(Pizza$driver, Pizza$operator, useNA = "ifany")
  b <- cramerV(Pizza$driver, Pizza$operator)
  expect_false(isTRUE(all.equal(a, b)))
})


test_that("the Fisher methods report NA where the transformation is undefined", {

  # n <= 3: Var(atanh(V)) ~ 1/(n-3) is infinite at 3 and negative below.
  # Before, n = 3 silently gave (0, 1) and n = 2 gave NaN with a bare
  # "NaNs produced" from sqrt() - two neighbours, two answers.
  small <- matrix(c(1, 1, 1, 0), nrow = 2)   # n = 3

  for (m in c("fisher", "fisheradj")) {
    expect_warning(ci <- cramerV(small, conf.level = 0.95, method = m),
                   "3 observations")
    expect_true(is.na(ci[["lci"]]))
    expect_true(is.na(ci[["uci"]]))
    # the estimate itself is still reported
    expect_equal(unname(ci["est"]), cramerV(small))
  }
})


test_that("a perfect association does not get a degenerate Fisher interval", {

  # atanh(1) is infinite, so the interval used to collapse to (1, 1) and
  # rule out every value below 1 - reachable with any perfect table
  perfect <- matrix(c(5, 0, 0, 5), nrow = 2)
  expect_equal(cramerV(perfect), 1)

  for (m in c("fisher", "fisheradj")) {
    expect_warning(ci <- cramerV(perfect, conf.level = 0.95, method = m),
                   "perfect association")
    expect_true(is.na(ci[["lci"]]))
  }

  # the noncentral methods have neither n-3 nor atanh and still deliver
  ci <- cramerV(perfect, conf.level = 0.95, method = "ncchisq")
  expect_false(is.na(ci[["lci"]]))
  expect_true(ci[["lci"]] <= 1 && ci[["uci"]] <= 1)
})


test_that("NA bounds survive the sides machinery", {

  small <- matrix(c(1, 1, 1, 0), nrow = 2)

  expect_warning(ci <- cramerV(small, conf.level = 0.95,
                               method = "fisher", sides = "left"))
  # the informative side is NA, the open side is still closed at 1
  expect_true(is.na(ci[["lci"]]))
  expect_equal(ci[["uci"]], 1)
})
