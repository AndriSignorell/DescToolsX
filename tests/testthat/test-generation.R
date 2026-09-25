test_that("generation() assigns the documented birth-year classes", {
  yrs <- c(1945, 1946, 1964, 1965, 1979, 1980, 1995, 1996, 2010, 2011, 2026)
  expect_identical(
    as.character(generation(yrs)),
    c(NA, "Babyboomer", "Babyboomer", "Gen X", "Gen X",
      "Millennial", "Millennial", "Gen Z", "Gen Z",
      "Gen Alpha", "Gen Alpha"))
})

test_that("generation() returns an ordered factor with all five levels", {
  g <- generation(1970)
  expect_true(is.ordered(g))
  expect_identical(levels(g),
                   c("Babyboomer", "Gen X", "Millennial", "Gen Z", "Gen Alpha"))
  expect_true(generation(1950) < generation(2000))
})

test_that("generation() maps NA and NaN to NA and keeps zero length", {
  expect_true(all(is.na(generation(c(NA, NaN)))))
  # plain logical NA: NA in, NA out - no error
  expect_true(is.na(generation(NA)))
  expect_true(all(is.na(generation(c(NA, NA)))))
  expect_true(is.ordered(generation(NA)))
  expect_length(generation(numeric(0)), 0L)
})
