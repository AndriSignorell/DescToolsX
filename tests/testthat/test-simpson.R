
library(testthat)


test_that("gini matches manual computation", {
  x <- c("A","A","B","C","C","C")
  
  p <- prop.table(table(x))
  expected <- 1 - sum(p^2)
  
  expect_equal(simpson(x, method = "gini"), expected)
})

test_that("hunter matches manual computation", {
  x <- c("A","A","B","C","C","C")
  
  tt <- table(x)
  N <- sum(tt)
  expected <- 1 - sum(tt * (tt - 1)) / (N * (N - 1))
  
  expect_equal(simpson(x, method = "hunter"), expected)
})

test_that("iqv matches manual computation", {
  x <- c("A","A","B","C","C","C")
  
  p <- prop.table(table(x))
  k <- length(p)
  expected <- (1 - sum(p^2)) * k / (k - 1)
  
  expect_equal(simpson(x, method = "iqv"), expected)
})

test_that("gini matches vegan::diversity", {
  skip_if_not_installed("vegan")
  
  x <- c("A","A","B","C","C","C")
  tt <- table(x)
  
  expected <- vegan::diversity(tt, index = "simpson")
  
  expect_equal(simpson(x, method = "gini"), expected)
})


test_that("handles NA correctly", {
  x <- c("A","A",NA,"B")
  
  expect_true(is.na(simpson(x, na.rm = FALSE)))
  expect_false(is.na(simpson(x, na.rm = TRUE)))
})



test_that("counts input works", {
  counts <- c(A = 2, B = 1, C = 3)
  
  expect_equal(
    simpson(counts, method = "gini"),
    simpson(rep(names(counts), counts), method = "gini")
  )
})

test_that("method argument works", {
  x <- c("A","B","C")
  
  expect_error(simpson(x, method = "invalid"))
})



test_that("edge cases handled", {
  expect_warning(simpson(character(0)),                  "Empty input")
  expect_warning(simpson("A", method = "hunter"),        "N >= 2")
  expect_warning(simpson(c("A","A"), method = "iqv"),    "K >= 2")
  expect_true(is.na(suppressWarnings(simpson(character(0)))))
  expect_true(is.na(suppressWarnings(simpson("A", method = "hunter"))))
  expect_true(is.na(suppressWarnings(simpson(c("A","A"), method = "iqv"))))
})


test_that("hunter matches vegan unbiased simpson", {
  skip_if_not_installed("vegan")

  data(BCI, package = "vegan")
  x <- BCI[1, ]

  # vegan's unbiased estimator
  expected <- vegan::simpson.unb(x)

  expect_equal(
    suppressWarnings(simpson(x, method = "hunter")),
    unname(expected),
    tolerance = 1e-12
  )
  
})


test_that("Hunter-Gaston is the N-corrected Gini-Simpson", {
  
  # the identity holds for every count vector
  for(tt in list(c(2, 1, 3), c(10, 1, 1, 1), c(5, 5), rep(1, 6))) {
    N <- sum(tt)
    gini <- simpson(tt, "gini")
    expect_equal(simpson(tt, "hunter"), gini * N / (N - 1))
  }
})


test_that("a single occupied category is homogeneous, not undefined", {
  
  # This used to return NA with a warning, although the sample is simply
  # perfectly homogeneous and the index is 0 - the same value "gini" gives.
  expect_equal(simpson(c(5, 0, 0), "hunter"), 0)
  expect_equal(simpson(c("A", "A", "A", "A", "A"), "hunter"), 0)
  expect_equal(simpson(c(5, 0, 0), "gini"), 0)
  
  # only the IQV genuinely needs K >= 2 ...
  expect_warning(res <- simpson(c(5, 0, 0), "iqv"), "K >= 2")
  expect_true(is.na(res))

  # ... and with the possible categories given, it is 0 as well
  expect_equal(simpson(c(5, 0, 0), "iqv", categories = 5), 0)
})


test_that("Hunter-Gaston is undefined for a single observation", {
  
  expect_warning(res <- simpson(c(1, 0), "hunter"), "N >= 2")
  expect_true(is.na(res))
})


test_that("all categories distinct gives the maximum", {
  
  expect_equal(simpson(rep(1, 6), "hunter"), 1)
  expect_equal(simpson(rep(1, 6), "iqv"), 1)
})


test_that("missing values are handled for every input type", {
  
  expect_true(is.na(simpson(c("A", "A", NA, "B"), "gini")))
  expect_true(is.na(simpson(c(2, NA, 3), "gini")))
  
  expect_equal(simpson(c("A", "A", NA, "B"), "gini", na.rm = TRUE),
               simpson(c("A", "A", "B"), "gini"))
})


test_that("empty and invalid input", {
  
  expect_warning(res <- simpson(numeric(0)), "Empty")
  expect_true(is.na(res))
  
  expect_warning(res <- simpson(c(0, 0, 0)), "Empty")
  expect_true(is.na(res))
  
  expect_error(simpson(c(1, -1)), "non-negative")
  expect_error(simpson(c(1.5, 2.5), "hunter"), "integer counts")
  expect_error(simpson(c(1, 2), na.rm = NA), "na.rm")
})


test_that("results are unnamed scalars", {
  
  res <- simpson(c(A = 2, B = 1, C = 3), "gini")
  expect_null(names(res))
  expect_length(res, 1L)
})


test_that("Hunter-Gaston, Gini-Simpson and IQV agree with the definitions", {
  
  counts <- c(A = 2, B = 1, C = 3)            # N = 6, K = 3
  
  expect_equal(simpson(counts, "gini"),   22 / 36)
  expect_equal(simpson(counts, "hunter"), 1 - 8 / 30)
  expect_equal(simpson(counts, "iqv"),    22 / 36 * 3 / 2)
  
  # Hunter-Gaston = Gini-Simpson * N / (N - 1)
  expect_equal(simpson(counts, "hunter"), simpson(counts, "gini") * 6 / 5)
  
  # observations and counts give the same result
  x <- rep(c("A", "B", "C"), counts)
  for (m in c("gini", "hunter", "iqv"))
    expect_equal(simpson(x, m), simpson(counts, m))
})


test_that("Hunter-Gaston agrees with vegan::simpson.unb", {
  skip_if_not_installed("vegan")
  counts <- c(12, 5, 0, 7, 1, 3)
  expect_equal(simpson(counts, "hunter"),
               unname(vegan::simpson.unb(counts)))
})


test_that("the IQV uses the possible categories when given", {
  
  x <- c("A", "A", "B", "B", "C", "C")     # uniform over 3 observed
  
  expect_equal(simpson(x, "iqv"), 1)
  expect_equal(simpson(x, "iqv", categories = 5), (2 / 3) * 5 / 4)
  expect_equal(simpson(x, "iqv", categories = LETTERS[1:5]),
               simpson(x, "iqv", categories = 5))
  
  # empty factor levels count as possible categories
  f <- factor(x, levels = LETTERS[1:5])
  expect_equal(simpson(f, "iqv", categories = levels(f)),
               simpson(x, "iqv", categories = 5))
  
  expect_error(simpson(x, "iqv", categories = 2), "smaller")
  expect_error(simpson(x, "iqv", categories = c("A", "B", "D")), "C")
  expect_error(simpson(x, "iqv", categories = 4.5), "whole number")
  expect_warning(simpson(x, "gini", categories = 5), "only used")
})


test_that("input checks", {
  
  expect_silent(simpson(c(0.2, 0.3, 0.5), "gini"))
  expect_error(simpson(c(0.2, 0.3, 0.5), "hunter"), "integer counts")

  # would be 1.5, outside [0, 1]
  expect_error(simpson(rep(0.5, 4), "hunter"), "integer counts")
  
  expect_error(simpson(c(1, Inf, 2)), "finite")
  expect_error(simpson(data.frame(a = 1:2, b = 3:4)), "exactly one row")
  expect_equal(simpson(data.frame(a = 2, b = 1, c = 3), "hunter"), 1 - 8 / 30)

  # a character column must not turn the counts into observations
  expect_error(simpson(data.frame(A = 2, B = "3"), "gini"), "numeric counts")

  # NA is not a possible category
  expect_error(simpson(c("A", "B"), "iqv", categories = c("A", "B", NA)),
               "missing values")

  # huge counts: no overflow in n_i (n_i - 1) or N (N - 1)
  expect_equal(simpson(c(1e200, 1e200), "hunter"), 0.5)
})
