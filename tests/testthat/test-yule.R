

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
  resQ <- yuleQ(m)
  expect_named(resQ, c("est","lci","uci"))
  expect_equal(resQ[["est"]], Q_expected, tolerance = 1e-12)
  
  ## --- yuleY estimate ---
  resY <- yuleY(m)
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
  
  res_left <- yuleQ(m3, sides = "left")
  expect_equal(res_left[["uci"]], 1)
  
  ## --- einseitig rechts ---
  res_right <- yuleQ(m3, sides = "right")
  expect_equal(res_right[["lci"]], -1)
  
  ## --- Zero-Cell mit Korrektur ---
  m4 <- matrix(c(10, 0,
                 5, 12), nrow = 2)
  
  res_corr <- yuleQ(m4, correction = TRUE)
  expect_true(is.finite(res_corr[["est"]]))
  
  ## --- Konsistenz Q & Y via OR ---
  expect_equal(resQ[["est"]], Q_expected, tolerance = 1e-12)
  expect_equal(resY[["est"]], Y_expected, tolerance = 1e-12)
})
