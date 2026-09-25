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



test_that("large/small do not read past the end when NAs dominate", {
  
  # The point of this test is the out-of-bounds read: k was capped at the
  # length BEFORE the NAs were stripped, so top_i_cpp() ran past the end
  # of a two-element vector. Assert the contents, not the ordering - both
  # functions return ascending, which is a separate contract and not what
  # is under test here.
  x <- c(1, 2, NA, NA, NA)
  
  expect_length(large(x, k = 5), 2L)
  expect_length(small(x, k = 5), 2L)
  expect_setequal(large(x, k = 5), c(1, 2))
  expect_setequal(small(x, k = 5), c(1, 2))
  
  expect_equal(max(large(x, k = 5)), 2)
  expect_equal(min(small(x, k = 5)), 1)
})




# Review 25.09.2026 ------------------------------------------------------------

test_that("NAs are placed like sort() does and never displace a value", {
  x <- c(1, NA, 3)
  # k was capped at the non-missing count before the NAs were added back
  expect_identical(large(x, k = 3, na.last = TRUE),  c(1, 3, NA))
  expect_identical(large(x, k = 3, na.last = FALSE), c(NA, 1, 3))
  expect_identical(small(x, k = 3, na.last = FALSE), c(NA, 1, 3))
  expect_identical(small(x, k = 3, na.last = TRUE),  c(1, 3, NA))
  # NA is the smallest for na.last = FALSE: not among the two largest
  expect_identical(large(x, k = 2, na.last = FALSE), c(1, 3))
  expect_identical(large(x, k = 2, na.last = TRUE),  c(3, NA))
  expect_identical(small(x, k = 2, na.last = FALSE), c(NA, 1))
  expect_identical(small(x, k = 2, na.last = TRUE),  c(1, 3))
})

test_that("the class survives when NAs are added", {
  d <- as.Date("2024-01-01") + c(5, NA, 1, 9)
  expect_s3_class(large(d, k = 4, na.last = FALSE), "Date")
  expect_s3_class(small(d, k = 4, na.last = TRUE), "Date")
  expect_identical(small(d, k = 4, na.last = TRUE),
                   as.Date("2024-01-01") + c(1, 5, 9, NA))
})

test_that("unique = TRUE counts frequencies and places NAs", {
  x <- c(2, 2, 5, 7, 7, 7, NA, NA)
  l <- large(x, k = 2, unique = TRUE)
  expect_equal(l$value, c(5, 7))
  expect_equal(l$frequency, c(1, 3))
  lt <- large(x, k = 2, unique = TRUE, na.last = TRUE)
  expect_equal(lt$value, c(7, NA))
  expect_equal(lt$frequency, c(3, 2))
  lf <- large(x, k = 4, unique = TRUE, na.last = FALSE)
  expect_equal(lf$value, c(NA, 2, 5, 7))
  s <- small(x, k = 2, unique = TRUE, na.last = FALSE)
  expect_equal(s$value, c(NA, 2))
  expect_equal(s$frequency, c(2, 2))
  st <- small(x, k = 5, unique = TRUE, na.last = TRUE)
  expect_equal(st$value, c(2, 5, 7, NA))
})

test_that("unique = TRUE keeps factor labels and Date class", {
  f <- factor(c("b", "a", "c", "c"), levels = c("a", "b", "c"))
  expect_identical(large(f, k = 1, unique = TRUE)$value, "c")
  expect_identical(small(f, k = 1, unique = TRUE)$value, "a")
  d <- as.Date("2024-01-01") + c(3, 1, 1)
  expect_s3_class(small(d, k = 2, unique = TRUE)$value, "Date")
})

test_that("highLow lists values with frequencies above one", {
  out <- highLow(c(1, 1, 2, 3, 1000, 1000, 1000), nlow = 2, nhigh = 2)
  expect_match(out, "lowest : 1 (2), 2", fixed = TRUE)
  expect_match(out, "1'000 (3)", fixed = TRUE)
})
