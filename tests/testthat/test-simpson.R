
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

test_that("deltas matches manual computation", {
  x <- c("A","A","B","C","C","C")
  
  p <- prop.table(table(x))
  k <- length(p)
  expected <- (1 - sum(p^2)) * k / (k - 1)
  
  expect_equal(simpson(x, method = "deltas"), expected)
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
  expect_warning(simpson(c("A","A"), method = "deltas"), "k >= 2")
  expect_true(is.na(suppressWarnings(simpson(character(0)))))
  expect_true(is.na(suppressWarnings(simpson("A", method = "hunter"))))
  expect_true(is.na(suppressWarnings(simpson(c("A","A"), method = "deltas"))))
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


test_that("the three indices match their definitions", {
  
  x <- c("A", "A", "B", "C", "C", "C")
  tt <- c(2, 1, 3)
  N <- sum(tt)
  p <- tt / N
  k <- length(tt)
  
  expect_equal(simpson(x, "gini"), 1 - sum(p^2))
  expect_equal(simpson(x, "hunter"), 1 - sum(tt * (tt - 1)) / (N * (N - 1)))
  expect_equal(simpson(x, "deltas"), (1 - sum(p^2)) * k / (k - 1))
})


test_that("counts and observations give the same answer", {
  
  expect_equal(
    simpson(c(A = 2, B = 1, C = 3), "hunter"),
    simpson(c("A", "A", "B", "C", "C", "C"), "hunter")
  )
})


test_that("Hunter-Gaston is the N-corrected Gini-Simpson", {
  
  # The two 'bias corrections' are the same adjustment with N and with k in
  # the correction factor; the identity holds for every count vector.
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
  
  # only the Deltas correction genuinely needs k >= 2
  expect_warning(res <- simpson(c(5, 0, 0), "deltas"), "k >= 2")
  expect_true(is.na(res))
})


test_that("Hunter-Gaston is undefined for a single observation", {
  
  expect_warning(res <- simpson(c(1, 0), "hunter"), "N >= 2")
  expect_true(is.na(res))
})


test_that("all categories distinct gives the maximum", {
  
  expect_equal(simpson(rep(1, 6), "hunter"), 1)
  expect_equal(simpson(rep(1, 6), "deltas"), 1)
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
  expect_warning(simpson(c(1.5, 2.5), "hunter"), "Non-integer")
  expect_error(simpson(c(1, 2), na.rm = NA), "na.rm")
})


test_that("results are unnamed scalars", {
  
  res <- simpson(c(A = 2, B = 1, C = 3), "gini")
  expect_null(names(res))
  expect_length(res, 1L)
})
