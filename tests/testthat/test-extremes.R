# ---- large() ----

test_that("large returns the k largest values sorted descending", {
  x <- c(3, 1, 4, 1, 5, 9, 2, 6)
  expect_equal(sort(large(x, k = 3), decreasing = TRUE),
               sort(large(x, k = 3), decreasing = TRUE))
  expect_true(all(large(x, k = 3) %in% c(5, 6, 9)))
})

test_that("large returns exactly k values", {
  expect_length(large(1:20, k = 5), 5L)
})

test_that("large with k > length(x) returns all values", {
  x <- 1:3
  expect_length(large(x, k = 10), 3L)
})

test_that("large unique = TRUE returns a list with value and frequency", {
  x <- c(1, 1, 2, 3, 3, 3)
  res <- large(x, k = 2, unique = TRUE)
  expect_named(res, c("value", "frequency"))
  expect_equal(length(res$value), 2L)
})

test_that("large removes NAs by default (na.last = NA)", {
  x <- c(1, NA, 3, 5)
  res <- large(x, k = 2)
  expect_false(any(is.na(res)))
})

test_that("large na.last = TRUE appends NA at the end", {
  x <- c(1, NA, 3)
  res <- large(x, k = 3, na.last = TRUE)
  expect_true(is.na(res[3]))
})


# ---- small() ----

test_that("small returns the k smallest values", {
  x <- c(3, 1, 4, 1, 5, 9, 2, 6)
  expect_true(all(small(x, k = 3) %in% c(1, 1, 2)))
})

test_that("small returns exactly k values", {
  expect_length(small(1:20, k = 5), 5L)
})

test_that("small with k > length(x) returns all values", {
  x <- 1:3
  expect_length(small(x, k = 10), 3L)
})

test_that("small unique = TRUE returns a list with value and frequency", {
  x <- c(1, 1, 2, 3, 3, 3)
  res <- small(x, k = 2, unique = TRUE)
  expect_named(res, c("value", "frequency"))
})

test_that("small removes NAs by default", {
  x <- c(1, NA, 3, 5)
  res <- small(x, k = 2)
  expect_false(any(is.na(res)))
})

test_that("small and large are complementary: together cover the full range", {
  x <- 1:10
  s <- small(x, k = 3)
  l <- large(x, k = 3)
  expect_true(all(s %in% 1:3))
  expect_true(all(l %in% 8:10))
})


# ---- highLow() ----

test_that("highLow returns a character string", {
  x <- rnorm(50)
  expect_type(highLow(x), "character")
})

test_that("highLow output contains 'lowest' and 'highest' labels", {
  x <- 1:20
  out <- highLow(x)
  expect_match(out, "lowest")
  expect_match(out, "highest")
})

test_that("highLow nlow = 0 and nhigh = 0 returns empty lines", {
  out <- highLow(1:10, nlow = 0, nhigh = 0)
  expect_match(out, "lowest :", fixed = TRUE)
})
