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


test_that("zodiac() respects the documented boundaries", {
  
  # first and last day of every sign, as given in the documentation
  bnd <- c("2015-01-19" = "Capricorn",  "2015-01-20" = "Aquarius",
           "2015-02-17" = "Aquarius",   "2015-02-18" = "Pisces",
           "2015-03-19" = "Pisces",     "2015-03-20" = "Aries",
           "2015-04-19" = "Aries",      "2015-04-20" = "Taurus",
           "2015-05-19" = "Taurus",     "2015-05-20" = "Gemini",
           "2015-06-20" = "Gemini",     "2015-06-21" = "Cancer",
           "2015-07-21" = "Cancer",     "2015-07-22" = "Leo",
           "2015-08-22" = "Leo",        "2015-08-23" = "Virgo",
           "2015-09-21" = "Virgo",      "2015-09-22" = "Libra",
           "2015-10-22" = "Libra",      "2015-10-23" = "Scorpio",
           "2015-11-21" = "Scorpio",    "2015-11-22" = "Sagittarius",
           "2015-12-21" = "Sagittarius","2015-12-22" = "Capricorn",
           "2015-12-31" = "Capricorn",  "2015-01-01" = "Capricorn")
  
  expect_equal(as.character(zodiac(as.Date(names(bnd)))), unname(bnd))
  
})


test_that("the result is a factor with 12 merged levels", {
  
  z <- zodiac(as.Date(c("2015-01-01", "2015-07-01")))
  expect_s3_class(z, "factor")
  expect_equal(nlevels(z), 12)
  expect_equal(levels(z)[1], "Capricorn")
  expect_false(anyDuplicated(levels(z)) > 0)
  
})


test_that("stringsAsFactors = FALSE gives the same labels as a character vector", {
  
  d <- as.Date(c("1937-07-28", "1936-06-01", "1966-02-25",
                 "1964-11-17", "1972-04-25"))
  expect_equal(zodiac(d, stringsAsFactors = FALSE),
               as.character(zodiac(d)))
  expect_type(zodiac(d, stringsAsFactors = FALSE), "character")
  
})


test_that("every day of a year is assigned to exactly one sign", {
  
  d <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "day")  # leap year
  z <- zodiac(d)
  expect_false(anyNA(z))
  expect_equal(length(unique(as.character(z))), 12)
  
  # the counts of the two Capricorn stretches add up
  expect_equal(sum(z == "Capricorn"), 19 + 10)
  
})


test_that("the German labels match the English ones position by position", {
  
  d <- as.Date(sprintf("2015-%02d-25", 1:12))
  expect_equal(as.integer(zodiac(d, lang = "de")),
               as.integer(zodiac(d, lang = "en")))
  expect_equal(as.character(zodiac(as.Date("2015-07-28"), lang = "de")), "Loewe")
  
})


test_that("NA dates propagate", {
  
  z <- zodiac(as.Date(c("2015-05-05", NA)))
  expect_true(is.na(z[2]))
  expect_false(is.na(z[1]))
  expect_true(is.na(zodiac(as.Date(NA), stringsAsFactors = FALSE)))
  
})


test_that("zodiac() validates its arguments", {
  
  expect_error(zodiac(as.Date("2015-01-01"), lang = "fr"))
  expect_error(zodiac(as.Date("2015-01-01"), stringsAsFactors = NA), "TRUE or FALSE")
  
})
