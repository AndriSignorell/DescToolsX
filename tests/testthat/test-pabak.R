
test_that("pabak works correctly", {
  
  # ------------------------------------------------------------------
  # Point estimate for 2x2 table
  # ------------------------------------------------------------------
  
  x <- matrix(c(45, 15,
                5, 35),
              nrow = 2, byrow = TRUE)
  
  # po = (45 + 35) / 100 = 0.8
  # PABAK = 2 * 0.8 - 1 = 0.6
  
  # as.vector() strips the "est" name and the diagnostic attributes
  expect_equal(
    as.vector(pabak(x)),
    0.6,
    tolerance = 1e-12
  )
  
  
  # ------------------------------------------------------------------
  # Confidence interval and diagnostic indices
  # ------------------------------------------------------------------
  
  res <- pabak(x, conf.level = 0.95)
  
  expect_named(
    res,
    c("est", "lci", "uci")
  )
  
  expect_equal(
    unname(res["est"]),
    0.6,
    tolerance = 1e-12
  )
  
  expect_equal(
    attr(res, "prevalenceIndex"),
    abs(45/100 - 35/100),
    tolerance = 1e-12
  )
  
  expect_equal(
    attr(res, "biasIndex"),
    abs((45 + 15)/100 - (45 + 5)/100),
    tolerance = 1e-12
  )

  # the diagnostics describe the table, not the interval, so they are
  # attached whether or not a confidence interval was requested
  expect_equal(
    attr(pabak(x), "prevalenceIndex"),
    attr(res, "prevalenceIndex")
  )

  expect_equal(
    attr(pabak(x), "biasIndex"),
    attr(res, "biasIndex")
  )

  expect_equal(attr(res, "nObs"), 100)

  # the result stays a plain 1- or 3-element vector
  expect_length(pabak(x), 1L)
  expect_length(res, 3L)
  
  
  # ------------------------------------------------------------------
  # PI and BI are undefined for k > 2
  # ------------------------------------------------------------------
  
  x3 <- diag(c(10, 20, 30))
  
  res3 <- pabak(x3, conf.level = 0.95)
  
  expect_true(is.na(attr(res3, "prevalenceIndex")))
  expect_true(is.na(attr(res3, "biasIndex")))
  
  
  # ------------------------------------------------------------------
  # Vector interface equals matrix interface
  # ------------------------------------------------------------------
  
  r1 <- factor(c("A", "A", "A", "B", "B"))
  r2 <- factor(c("A", "A", "B", "B", "B"))
  
  expect_equal(
    pabak(r1, r2),
    pabak(table(r1, r2)),
    tolerance = 1e-12
  )
  
  
  # ------------------------------------------------------------------
  # Confidence interval is truncated to [-1, 1]
  # ------------------------------------------------------------------
  
  xPerfect <- matrix(c(99, 0,
                       0, 1),
                     nrow = 2, byrow = TRUE)
  
  resPerfect <- pabak(
    xPerfect,
    conf.level = 0.999
  )
  
  expect_gte(resPerfect["lci"], -1)
  expect_lte(resPerfect["uci"],  1)
  
  
  # ------------------------------------------------------------------
  # One-sided confidence intervals
  # ------------------------------------------------------------------
  
  resLeft <- pabak(
    x,
    conf.level = 0.95,
    sides = "left"
  )
  
  resRight <- pabak(
    x,
    conf.level = 0.95,
    sides = "right"
  )
  
  # sides names the side on which the finite bound lies: "left" gives
  # [lci, Inf), "right" gives (-Inf, uci]
  expect_true(is.finite(resLeft["lci"]))
  expect_true(is.finite(resRight["uci"]))
  
  expect_equal(unname(resLeft["uci"]), 1)     # PABAK <= 1
  expect_equal(unname(resRight["lci"]), -1)   # PABAK >= -1
  
  # one-sided limits are tighter than the two-sided ones
  expect_gt(resLeft["lci"], res["lci"])
  expect_lt(resRight["uci"], res["uci"])
  
  
  # ------------------------------------------------------------------
  # Empty confusion matrix
  # ------------------------------------------------------------------
  
  expect_error(
    pabak(matrix(0, 2, 2)),
    "empty"
  )
  
  
  # ------------------------------------------------------------------
  # Invalid confidence levels
  # ------------------------------------------------------------------
  
  expect_error(
    pabak(x, conf.level = 0)
  )
  
  expect_error(
    pabak(x, conf.level = 1)
  )
  
  expect_error(
    pabak(x, conf.level = 1.1)
  )
  
  expect_error(
    pabak(x, conf.level = c(0.9, 0.95))
  )
  
})


test_that("pabak reports the open side at the range boundary", {
  
  m <- as.table(matrix(c(40, 10, 8, 42), nrow = 2,
                       dimnames = list(c("no", "yes"), c("no", "yes"))))
  
  left  <- pabak(m, conf.level = 0.95, sides = "left")
  right <- pabak(m, conf.level = 0.95, sides = "right")
  
  # PABAK lies in [-1, 1] and the two-sided bounds are truncated to it
  expect_equal(unname(left[["uci"]]), 1)
  expect_equal(unname(right[["lci"]]), -1)
  expect_true(is.finite(left[["lci"]]))
  
  # and the estimate is 2*po - 1. ignore_attr: the result carries nObs,
  # prevalenceIndex and biasIndex, which unname() does not remove.
  expect_equal(pabak(m), 2 * (82 / 100) - 1, ignore_attr = TRUE)
})

