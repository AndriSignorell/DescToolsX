
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
