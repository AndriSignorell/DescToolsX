

test_that("cohenH has exact known values and changes sign under row reversal", {
  tab <- matrix(c(3, 1, 1, 3), 2, byrow = TRUE)
  expect_equal(unname(cohenH(tab)), pi / 3)
  expect_equal(unname(cohenH(tab[2:1, ])), -pi / 3)
  expect_equal(unname(cohenH(tab[, 2:1])), -pi / 3)
  expect_equal(unname(cohenH(matrix(5, 2, 2))), 0)
  expect_equal(unname(cohenH(diag(c(10, 10)))), pi)
})

test_that("cohenH intervals use the independent-proportions asymptotic SE", {
  tab <- matrix(c(30, 10, 10, 30), 2, byrow = TRUE)
  h <- pi / 3
  se <- sqrt(1 / 40 + 1 / 40)
  expect_equal(cohenH(tab, conf.level = 0.95),
               c(est = h, lci = h - qnorm(0.975) * se, uci = h + qnorm(0.975) * se))
  # h lies in [-pi, pi]: the open side is reported at that boundary
  expect_equal(cohenH(tab, conf.level = 0.95, sides = "left"),
               c(est = h, lci = h - qnorm(0.95) * se, uci = pi))
  expect_equal(cohenH(tab, conf.level = 0.95, sides = "right"),
               c(est = h, lci = -pi, uci = h + qnorm(0.95) * se))
})

test_that("cohenH accepts vectors with an explicitly defined event order", {
  group <- factor(rep(c("A", "B"), each = 4), levels = c("A", "B"))
  event <- factor(c("yes", "yes", "yes", "no", "yes", "no", "no", "no"),
                  levels = c("yes", "no"))
  expect_equal(unname(cohenH(group, event)), pi / 3)
  expect_equal(cohenH(group, event, conf.level = 0.9),
               cohenH(table(group, event), conf.level = 0.9))
})

test_that("cohenH checks dimensions, empty rows and scalar interval levels", {
  expect_error(cohenH(1:4), "2x2 table")
  expect_error(cohenH(matrix(1, 2, 3)), "2x2 table")
  expect_error(cohenH(array(1, c(2, 2, 2))), "2x2 table")
  expect_error(cohenH(matrix(letters[1:4], 2)), "numeric")
  expect_error(cohenH(matrix(c(0, 0, 2, 3), 2, byrow = TRUE)), "Both rows")
  expect_error(cohenH(matrix(c(2, 3, 0, 0), 2, byrow = TRUE)), "Both rows")
  for (bad in list("0.95", 0, 1, Inf))
    expect_error(cohenH(matrix(1, 2, 2), conf.level = bad), "conf.level")
  expect_error(cohenH(matrix(1, 2, 2), sides = "invalid"), "arg")
})


# Additional cases ---------------------------------------------------------------

# h = 2 asin(sqrt(p1)) - 2 asin(sqrt(p2)), p_i = row-wise share of column 1
hRef <- function(m)
  2 * asin(sqrt(m[1, 1] / sum(m[1, ]))) - 2 * asin(sqrt(m[2, 1] / sum(m[2, ])))

tab <- matrix(c(26, 26,
                 6,  7), nrow = 2, byrow = TRUE,
              dimnames = list(c("A", "B"), c("yes", "no")))

test_that("cohenH reproduces the arcsine difference for asymmetric tables", {
  expect_equal(as.numeric(cohenH(tab)),
               2 * asin(sqrt(26 / 52)) - 2 * asin(sqrt(6 / 13)))
  for (m in list(matrix(c(55, 45, 45, 55), 2, byrow = TRUE),
                 matrix(c(70, 30, 20, 80), 2, byrow = TRUE)))
    expect_equal(as.numeric(cohenH(m)), hRef(m))
  expect_equal(as.numeric(cohenH(matrix(c(0, 100, 100, 0), 2))), -pi)
})

test_that("negative, missing and infinite counts are refused", {
  # rows (-5, -5) and (5, 5) gave p1 = p2 = 0.5 and h = 0
  expect_error(cohenH(matrix(c(-5, 5, -5, 5), 2)), "non-negative")
  expect_error(cohenH(matrix(c(1, NA, 3, 4), 2)), "missing")
  expect_error(cohenH(matrix(c(1, Inf, 3, 4), 2)), "infinite")
})

test_that("the interval is clamped to [-pi, pi]", {
  res <- cohenH(diag(c(100, 100)), conf.level = 0.95)
  expect_equal(unname(res[["est"]]), pi)
  expect_equal(unname(res[["uci"]]), pi)      # h + z * se would exceed pi
  two <- cohenH(tab, conf.level = 0.90)
  expect_equal(unname(cohenH(tab, conf.level = 0.95, sides = "left")[["lci"]]),
               unname(two[["lci"]]))
})

test_that("conf.level is validated before use", {
  for (cl in list(NULL, NaN, c(0.9, 0.95)))
    expect_error(cohenH(tab, conf.level = cl), "conf.level")
  expect_error(cohenH(tab, conf.level = 0.4, sides = "left"), "exceed 0.5")
})
