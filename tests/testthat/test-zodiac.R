test_that("zodiac returns a factor by default", {
  d <- as.Date("1990-03-21")
  expect_s3_class(zodiac(d), "factor")
})

test_that("zodiac returns character when stringsAsFactors = FALSE", {
  d <- as.Date("1990-03-21")
  expect_type(zodiac(d, stringsAsFactors = FALSE), "character")
})

test_that("zodiac assigns Aries for March 20 - April 19", {
  expect_equal(as.character(zodiac(as.Date("2000-04-01"), lang="en")), "Aries")
})

test_that("zodiac assigns Capricorn for December 25", {
  expect_equal(as.character(zodiac(as.Date("2000-12-25"), lang="en")), "Capricorn")
})

test_that("zodiac assigns Capricorn for January 10", {
  expect_equal(as.character(zodiac(as.Date("2000-01-10"), lang="en")), "Capricorn")
})

test_that("zodiac assigns Aquarius for January 25", {
  expect_equal(as.character(zodiac(as.Date("2000-01-25"), lang="en")), "Aquarius")
})

test_that("zodiac lang = 'de' returns German names", {
  d <- as.Date("2000-04-01")
  res <- zodiac(d, lang = "de", stringsAsFactors = FALSE)
  expect_equal(res, "Widder")
})

test_that("zodiac is vectorised over x", {
  dates <- as.Date(c("2000-01-15","2000-04-01","2000-07-01"))
  res <- zodiac(dates, stringsAsFactors = FALSE)
  expect_length(res, 3)
  expect_equal(res, c("Capricorn","Aries","Cancer"))
})

test_that("zodiac factor has 12 levels", {
  dates <- seq(as.Date("2000-01-01"), as.Date("2000-12-31"), by = "week")
  res <- zodiac(dates)
  expect_equal(nlevels(res), 12L)
})
