
test_that("pabak works correctly", {
  
  # ------------------------------------------------------------------
  # Point estimate for 2x2 table
  # ------------------------------------------------------------------
  
  x <- matrix(c(45, 15,
                5, 35),
              nrow = 2, byrow = TRUE)
  
  # po = (45 + 35) / 100 = 0.8
  # PABAK = 2 * 0.8 - 1 = 0.6
  
  expect_equal(
    pabak(x),
    0.6,
    tolerance = 1e-12
  )
  
  
  # ------------------------------------------------------------------
  # Confidence interval and diagnostic indices
  # ------------------------------------------------------------------
  
  res <- pabak(x, conf.level = 0.95)
  
  expect_named(
    res,
    c("est", "lci", "uci", "pi", "bi")
  )
  
  expect_equal(
    unname(res["est"]),
    0.6,
    tolerance = 1e-12
  )
  
  expect_equal(
    unname(res["pi"]),
    abs(45/100 - 35/100),
    tolerance = 1e-12
  )
  
  expect_equal(
    unname(res["bi"]),
    abs((45 + 15)/100 - (45 + 5)/100),
    tolerance = 1e-12
  )
  
  
  # ------------------------------------------------------------------
  # PI and BI are undefined for k > 2
  # ------------------------------------------------------------------
  
  x3 <- diag(c(10, 20, 30))
  
  res3 <- pabak(x3, conf.level = 0.95)
  
  expect_true(is.na(res3["pi"]))
  expect_true(is.na(res3["bi"]))
  
  
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
  
  expect_true(is.infinite(resLeft["uci"]))
  expect_true(is.infinite(resRight["lci"]))
  
  
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