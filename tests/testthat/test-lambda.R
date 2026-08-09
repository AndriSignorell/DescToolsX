m <- as.table(cbind(c(1768, 946, 115), c(807, 1387, 438),
                    c(189, 746, 288), c(47, 53, 16)))
dimnames(m) <- list(paste("A", 1:3), paste("B", 1:4))


test_that("lambda() validates conf.level before it is used", {

  expect_silent(lambda(m))
  expect_length(lambda(m, conf.level = NA), 1L)

  # these used to abort inside if(is.na(conf.level)), with a message about
  # the condition rather than about the argument
  expect_error(lambda(m, conf.level = c(0.9, 0.95)), "conf.level")
  expect_error(lambda(m, conf.level = NULL), "conf.level")

  # NaN used to slip through and return the point estimate
  expect_error(lambda(m, conf.level = NaN), "conf.level")

  expect_error(lambda(m, conf.level = "0.95"), "conf.level")
  expect_error(lambda(m, conf.level = 0), "conf.level")
  expect_error(lambda(m, conf.level = 1), "conf.level")
})


test_that("sides and method are matched even without an interval", {

  # both used to be unmatched when conf.level was NA: 'sides' was matched
  # inside the interval branch, 'method' was never matched at all
  expect_error(lambda(m, sides = "links"), "two.sided")

  expect_error(lambda(m, direction = "diagonal"), "symmetric")
})


test_that("the point estimate does not depend on sides", {

  for (d in c("symmetric", "row", "column")) {
    v <- lambda(m, direction = d)
    for (s in c("two.sided", "left", "right"))
      expect_equal(unname(lambda(m, direction = d,
                                 conf.level = 0.95, sides = s)["est"]), v)
  }
})


test_that("sides names the side carrying the finite bound", {

  for (d in c("symmetric", "row", "column")) {

    two   <- lambda(m, direction = d, conf.level = 0.95)
    left  <- lambda(m, direction = d, conf.level = 0.95, sides = "left")
    right <- lambda(m, direction = d, conf.level = 0.95, sides = "right")

    expect_equal(left[["uci"]], 1, info = d)
    expect_equal(right[["lci"]], 0, info = d)

    expect_gte(left[["lci"]],  two[["lci"]])
    expect_lte(right[["uci"]], two[["uci"]])

    # left(gamma) reads the same end as two.sided(2*gamma - 1)
    two90 <- lambda(m, direction = d, conf.level = 0.90)
    expect_equal(left[["lci"]], two90[["lci"]], info = d)
    expect_equal(right[["uci"]], two90[["uci"]], info = d)
  }
})


test_that("a one-sided interval below conf.level 0.5 is refused", {

  # the adjusted level would be non-positive, qnorm() negative, and the
  # two bounds came out reversed - pmin/pmax clamped them elementwise and
  # did not notice
  expect_error(lambda(m, conf.level = 0.4, sides = "left"), "0.5")
  expect_error(lambda(m, conf.level = 0.5, sides = "right"), "0.5")

  expect_silent(lambda(m, conf.level = 0.4))
})


test_that("bounds stay inside [0, 1] and bracket the estimate", {

  for (d in c("symmetric", "row", "column")) {
    ci <- lambda(m, direction = d, conf.level = 0.95)
    expect_named(ci, c("est", "lci", "uci"))
    expect_true(ci[["lci"]] >= 0 && ci[["uci"]] <= 1, info = d)
    expect_true(ci[["lci"]] <= ci[["est"]] && ci[["est"]] <= ci[["uci"]],
                info = d)
  }
})


test_that("the non-square branches still work", {

  # regression from the earlier round: L.col/L.row were allocated with
  # swapped lengths, so a non-square table aborted with "subscript out of
  # bounds" as soon as conf.level was supplied
  expect_silent(lambda(m, direction = "column", conf.level = 0.95))
  expect_silent(lambda(m, direction = "row", conf.level = 0.95))
  expect_silent(lambda(t(m), direction = "column", conf.level = 0.95))
  expect_silent(lambda(t(m), direction = "row", conf.level = 0.95))
})


test_that("... reaches the table constructor for x and y", {

  # regression: `...` used to be forwarded only when y was given, so
  # useNA was silently dropped for a table input
  x <- c("a", "a", "b", NA, "b", "a")
  y <- c("u", "v", "u", "v", NA, "u")

  a <- lambda(x, y, useNA = "ifany")
  b <- lambda(x, y)
  expect_false(isTRUE(all.equal(a, b)))
})
