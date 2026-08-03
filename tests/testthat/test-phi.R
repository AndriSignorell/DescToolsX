
test_that("phi() equals sqrt(chi2 / n)", {

  tab <- matrix(c(10, 20, 30, 40), nrow = 2)
  expect_equal(phi(tab), 200 / sqrt(40 * 60 * 30 * 70), tolerance = 1e-8)
  expect_equal(phi(tab),
               sqrt(suppressWarnings(
                 chisq.test(tab, correct = FALSE)$statistic) / sum(tab)),
               ignore_attr = TRUE)
})


test_that("phi() is unsigned", {

  tab <- matrix(c(10, 20, 30, 40), nrow = 2)
  signedPhi <- (tab[1, 1] * tab[2, 2] - tab[1, 2] * tab[2, 1]) /
    sqrt(prod(rowSums(tab), colSums(tab)))

  expect_lt(signedPhi, 0)
  expect_equal(phi(tab), abs(signedPhi), tolerance = 1e-8)

  # mirroring the table leaves the reported value unchanged
  expect_equal(phi(tab), phi(tab[, 2:1]), tolerance = 1e-8)
})


test_that("phi() builds the table from two vectors", {

  x <- c("A", "A", "B", "B")
  y <- c("yes", "no", "yes", "no")
  expect_equal(phi(x, y), 0)
  expect_equal(phi(x, y), phi(table(x, y)))
})


test_that("phi() validates its input", {

  expect_error(phi(1:4), "two-dimensional")
  expect_error(phi(matrix(c(1, -1, 2, 3), nrow = 2)), "non-negative")
  expect_error(phi(matrix(c(1, NA, 2, 3), nrow = 2)), "non-negative")
  expect_error(phi(matrix(0, nrow = 2, ncol = 2)), "at least one observation")
})
