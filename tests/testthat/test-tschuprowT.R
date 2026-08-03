test_that("tschuprowT returns a single numeric", {
  tab <- matrix(c(10, 20, 30, 40), nrow=2)
  expect_length(tschuprowT(tab), 1)
  expect_true(is.numeric(tschuprowT(tab)))
})

test_that("tschuprowT is 0 for an independent table", {
  tab <- matrix(rep(25,4), nrow=2)
  expect_equal(tschuprowT(tab), 0, tolerance=1e-10)
})

test_that("tschuprowT is 1 for perfect association in a 2x2 table", {
  tab <- matrix(c(50,0,0,50), nrow=2)
  expect_equal(tschuprowT(tab), 1, tolerance=1e-10)
})

test_that("tschuprowT is non-negative", {
  tab <- matrix(c(10,20,30,40), nrow=2)
  expect_gte(tschuprowT(tab), 0)
})

test_that("tschuprowT equals cramerV for square tables", {
  # For square tables: sqrt((r-1)(c-1)) = r-1 = c-1 → T = V
  tab <- matrix(c(40,10,10,40), nrow=2)
  expect_equal(tschuprowT(tab), cramerV(as.table(tab)), tolerance=1e-8)
})

test_that("tschuprowT correct = TRUE returns value in [0, 1]", {
  tab <- matrix(c(10,5,5,30), nrow=2)
  res <- tschuprowT(tab, correct=TRUE)
  expect_gte(res, 0); expect_lte(res, 1)
})

test_that("tschuprowT accepts two vectors", {
  x <- c("A","A","B","B"); y <- c("yes","no","yes","no")
  expect_length(tschuprowT(x, y), 1)
})

test_that("tschuprowT manual formula matches function", {
  tab <- matrix(c(10,5,5,20), nrow=2)
  chi2 <- chisq.test(tab, correct=FALSE)$statistic
  df   <- prod(dim(tab)-1)
  expected <- sqrt(unname(chi2)/(sum(tab)*sqrt(df)))
  expect_equal(tschuprowT(tab), expected, tolerance=1e-10)
})


test_that("tschuprowT() reproduces the closed form", {
  
  tab <- matrix(c(10, 20, 30, 40), nrow = 2)
  
  # chi2 = n(ad-bc)^2 / (r1 r2 c1 c2) = 100*200^2/(40*60*30*70)
  chi2 <- 100 * (10*40 - 30*20)^2 / (40 * 60 * 30 * 70)
  expect_equal(tschuprowT(tab), sqrt(chi2 / (100 * sqrt(1))))
  expect_equal(tschuprowT(tab), 0.08908708, tolerance = 1e-6)
  
  # 3x4 table
  m <- as.table(cbind(c(1768, 946, 115), c(807, 1387, 438),
                      c(189, 746, 288), c(47, 53, 16)))
  chi <- suppressWarnings(chisq.test(m, correct = FALSE)$statistic)
  expect_equal(tschuprowT(m),
               as.numeric(sqrt(chi / (sum(m) * sqrt(2 * 3)))))
  
})


test_that("for a 2x2 table T equals |phi| and Cramer's V", {
  
  tab <- matrix(c(13, 7, 4, 26), nrow = 2)
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  expect_equal(tschuprowT(tab), as.numeric(sqrt(chi / sum(tab))))
  
})


test_that("the vector interface tabulates", {
  
  x <- c("A", "A", "B", "B", "A", "B")
  y <- c("yes", "no", "yes", "no", "no", "no")
  expect_equal(tschuprowT(x, y), tschuprowT(table(x, y)))
  
})


test_that("Bergsma's bias correction follows the published formula", {
  
  m <- as.table(cbind(c(17, 9, 5), c(8, 13, 4), c(2, 7, 12)))
  n <- sum(m)
  d <- dim(m)
  phi2  <- as.numeric(suppressWarnings(chisq.test(m, correct = FALSE)$statistic)) / n
  phi2c <- max(0, phi2 - prod(d - 1) / (n - 1))
  dc    <- d - (d - 1)^2 / (n - 1)
  
  expect_equal(tschuprowT(m, correct = TRUE),
               sqrt(phi2c / sqrt(prod(dc - 1))))
  
  # the correction can only shrink the estimate
  expect_lte(tschuprowT(m, correct = TRUE), tschuprowT(m))
  
  # here the correction exceeds the estimate
  expect_equal(tschuprowT(matrix(c(10, 20, 30, 40), nrow = 2), correct = TRUE), 0)
  
})


test_that("one-dimensional input is rejected instead of being run through a GOF test", {
  
  # regression: chisq.test() would happily perform a goodness-of-fit test on
  # a vector and tschuprowT() returned a number unrelated to any association
  expect_error(tschuprowT(c(10, 20, 30)), "two-dimensional")
  expect_error(tschuprowT(as.table(c(a = 10, b = 20))), "two-dimensional")
  
})


test_that("invalid tables are rejected", {
  
  expect_error(tschuprowT(matrix(c(1, 2, NA, 4), nrow = 2)), "non-negative counts")
  expect_error(tschuprowT(matrix(c(1, 2, -3, 4), nrow = 2)), "non-negative counts")
  expect_error(tschuprowT(matrix(c(0, 0, 0, 1), nrow = 2) * 0), "at least 2")
  expect_error(tschuprowT(matrix(1:4, nrow = 2), correct = NA), "TRUE or FALSE")
  
})


test_that("a degenerate table gives NA rather than a number", {
  
  expect_true(is.na(tschuprowT(matrix(c(4, 6), nrow = 1))))
  expect_true(is.na(tschuprowT(matrix(c(4, 6), ncol = 1))))
  
})
