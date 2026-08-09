

test_that("yuleQ and yuleY work correctly", {
  
  ## --- Basis-Matrix ---
  # byrow = TRUE so the literal layout matches the a/b/c/d labelling below;
  # OR is invariant to swapping b and c, but the default column-major fill
  # would put 5 in cell c and 3 in cell b.
  m <- matrix(c(12, 5,
                3, 20), nrow = 2, byrow = TRUE)
  
  a <- 12; b <- 5; c <- 3; d <- 20
  OR <- (a*d)/(b*c)
  
  Q_expected <- (OR - 1)/(OR + 1)
  # Yule's Y is tanh(log(OR)/4); tanh(log(OR)/2) is algebraically
  # identical to Q, so using it here compared Y against Q.
  Y_expected <- (sqrt(OR) - 1)/(sqrt(OR) + 1)
  
  ## --- yuleQ estimate ---
  resQ <- yuleQ(m, conf.level = 0.95)
  expect_named(resQ, c("est","lci","uci"))
  expect_equal(resQ[["est"]], Q_expected, tolerance = 1e-12)
  
  ## --- yuleY estimate ---
  resY <- yuleY(m, conf.level = 0.95)
  expect_named(resY, c("est","lci","uci"))
  expect_equal(resY[["est"]], Y_expected, tolerance = 1e-12)
  
  ## --- zweiseitiges KI ---
  m2 <- matrix(c(10, 4,
                 3, 15), nrow = 2)
  
  resQ2 <- yuleQ(m2, conf.level = 0.95)
  expect_true(resQ2[["lci"]] <= resQ2[["est"]])
  expect_true(resQ2[["uci"]] >= resQ2[["est"]])
  
  ## --- einseitig links ---
  m3 <- matrix(c(8, 2,
                 1, 9), nrow = 2)
  
  res_left <- yuleQ(m3, conf.level = 0.95, sides = "left")
  expect_equal(res_left[["uci"]], 1)
  
  ## --- einseitig rechts ---
  res_right <- yuleQ(m3, conf.level = 0.95, sides = "right")
  expect_equal(res_right[["lci"]], -1)
  
  ## --- Zero-Cell mit Korrektur ---
  m4 <- matrix(c(10, 0,
                 5, 12), nrow = 2)
  
  res_corr <- yuleQ(m4, correct = TRUE, conf.level = 0.95)
  expect_true(is.finite(res_corr[["est"]]))
  
  ## --- Konsistenz Q & Y via OR ---
  expect_equal(resQ[["est"]], Q_expected, tolerance = 1e-12)
  expect_equal(resY[["est"]], Y_expected, tolerance = 1e-12)
})


m <- matrix(c(12, 5, 3, 20), nrow = 2)   # OR = 12*20/(3*5) = 16


test_that("yuleQ() and yuleY() reproduce the closed forms", {
  
  OR <- 12 * 20 / (3 * 5)
  
  expect_equal(yuleQ(m, conf.level = NA), (OR - 1)/(OR + 1))
  expect_equal(yuleQ(m, conf.level = NA), 15/17)
  
  expect_equal(yuleY(m, conf.level = NA), (sqrt(OR) - 1)/(sqrt(OR) + 1))
  expect_equal(yuleY(m, conf.level = NA), 0.6)
  
  # Y = tanh(atanh(Q)/2)
  expect_equal(yuleY(m, conf.level = NA),
               tanh(atanh(yuleQ(m, conf.level = NA))/2))
  
})


test_that("a zero cell yields the limiting value instead of NaN", {
  
  # regression: Q was computed as (OR-1)/(OR+1) with OR = Inf, giving NaN,
  # while the doc gives the stable form tanh(log(OR)/2) - which yuleY used
  z1 <- matrix(c(12, 5, 0, 20), nrow = 2)    # OR = Inf
  expect_equal(yuleQ(z1, conf.level = NA), 1)
  expect_equal(yuleY(z1, conf.level = NA), 1)
  
  z0 <- matrix(c(0, 5, 3, 20), nrow = 2)     # OR = 0
  expect_equal(yuleQ(z0, conf.level = NA), -1)
  expect_equal(yuleY(z0, conf.level = NA), -1)
  
  # ... and the interval does not contain NaN either
  ci <- yuleQ(z1, conf.level = 0.95)
  expect_false(anyNA(ci))
  expect_equal(unname(ci), c(1, -1, 1))
  
  # both diagonal products zero -> undefined
  expect_true(is.na(yuleQ(matrix(c(0, 5, 0, 20), nrow = 2), conf.level = NA)))
  
})


test_that("est lies inside the confidence interval", {
  
  for(f in list(yuleQ, yuleY)){
    ci <- f(m, conf.level = 0.95)
    expect_named(ci, c("est", "lci", "uci"))
    expect_lte(ci[["lci"]], ci[["est"]])
    expect_lte(ci[["est"]], ci[["uci"]])
    expect_gte(ci[["lci"]], -1)
    expect_lte(ci[["uci"]], 1)
  }
  
})


test_that("the interval is the transformed log odds ratio interval", {
  
  se <- sqrt(1/12 + 1/3 + 1/5 + 1/20)
  lo <- log(16) - qnorm(0.975) * se
  hi <- log(16) + qnorm(0.975) * se
  
  expect_equal(unname(yuleQ(m, conf.level = 0.95)[["lci"]]), tanh(lo/2))
  expect_equal(unname(yuleQ(m, conf.level = 0.95)[["uci"]]), tanh(hi/2))
  expect_equal(unname(yuleY(m, conf.level = 0.95)[["lci"]]), tanh(lo/4))
  expect_equal(unname(yuleY(m, conf.level = 0.95)[["uci"]]), tanh(hi/4))
  
})


test_that("one-sided intervals report the open side at the range limit", {
  
  left  <- yuleQ(m, conf.level = 0.95, sides = "left")
  right <- yuleQ(m, conf.level = 0.95, sides = "right")
  two   <- yuleQ(m, conf.level = 0.95)
  
  expect_equal(left[["uci"]], 1)
  expect_equal(right[["lci"]], -1)
  expect_gt(left[["lci"]], two[["lci"]])
  expect_lt(right[["uci"]], two[["uci"]])
  
})


test_that("the Haldane-Anscombe correction gives a finite interval", {
  
  z1 <- matrix(c(12, 5, 0, 20), nrow = 2)
  ci <- yuleQ(z1, correct = TRUE, conf.level = 0.95)
  
  expect_lt(ci[["est"]], 1)
  expect_gt(ci[["lci"]], -1)
  expect_lte(ci[["lci"]], ci[["est"]])
  
  # with a corrected table the estimate equals the one of the shifted table
  expect_equal(ci[["est"]], yuleQ(z1 + 0.5, conf.level = NA))
  
})


test_that("the vector interface tabulates", {
  
  x <- c("a", "a", "b", "b", "a", "b", "a", "b")
  y <- c("u", "v", "u", "v", "u", "u", "v", "v")
  expect_equal(yuleQ(x, y), yuleQ(table(x, y)))
  
})


test_that("yule coefficients validate their input", {
  
  expect_error(yuleQ(matrix(1:6, nrow = 2)), "2x2")
  expect_error(yuleQ(1:4), "2x2")
  expect_error(yuleQ(matrix(c(1, 2, -3, 4), nrow = 2)), "non-negative")
  expect_error(yuleQ(m, conf.level = 0), "conf.level")
  expect_error(yuleQ(m, correct = NA), "TRUE or FALSE")
  expect_error(yuleY(m, conf.level = 0.4, sides = "right"), "0.5")
  
})


test_that("yuleQ/yuleY return a bare estimate by default", {

  # The default moved from 0.95 to NA, in line with the rest of the suite.
  # That change is SILENT for anyone reading res[["est"]] - it starts
  # returning NA instead of failing - so it gets pinned here.
  expect_length(yuleQ(m), 1L)
  expect_length(yuleY(m), 1L)
  expect_null(names(yuleQ(m)))

  expect_identical(formals(yuleQ)$conf.level, NA)
  expect_identical(formals(yuleY)$conf.level, NA)
})
