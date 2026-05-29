test_that("countWorkDays returns 5 for a standard Mon-Fri week", {
  # 2023-01-02 is Monday, 2023-01-06 is Friday
  expect_equal(countWorkDays(as.Date("2023-01-02"), as.Date("2023-01-06")), 5L)
})

test_that("countWorkDays returns 1 when from == to on a workday (endpoint counted)", {
  # 2023-01-02 is Monday — the endpoint itself counts as 1 workday
  expect_equal(countWorkDays(as.Date("2023-01-02"), as.Date("2023-01-02")), 1)
})

test_that("countWorkDays excludes weekends", {
  # 2023-01-02 (Mon) to 2023-01-08 (Sun): 5 workdays
  expect_equal(countWorkDays(as.Date("2023-01-02"), as.Date("2023-01-08")), 5L)
})

test_that("countWorkDays excludes holiday within the period", {
  # Mon to Fri minus one holiday on Wednesday
  from    <- as.Date("2023-01-02")
  to      <- as.Date("2023-01-06")
  holiday <- as.Date("2023-01-04")   # Wednesday
  expect_equal(countWorkDays(from, to, holiday = holiday), 4L)
})

test_that("countWorkDays holiday outside range has no effect", {
  from    <- as.Date("2023-01-02")
  to      <- as.Date("2023-01-06")
  holiday <- as.Date("2023-01-09")   # following Monday, outside window
  expect_equal(countWorkDays(from, to, holiday = holiday), 5L)
})

test_that("countWorkDays returns a numeric vector", {
  res <- countWorkDays(as.Date("2023-01-02"), as.Date("2023-01-06"))
  expect_true(is.numeric(res))
})

test_that("countWorkDays is vectorised over from and to", {
  from <- rep(as.Date("2023-01-02"), 3)
  to   <- as.Date(c("2023-01-06", "2023-01-13", "2023-01-20"))
  res  <- countWorkDays(from, to)
  expect_equal(res, c(5L, 10L, 15L))
})

test_that("countWorkDays handles a two-week span correctly", {
  # 2 full Mon-Fri weeks = 10 workdays
  expect_equal(countWorkDays(as.Date("2023-01-02"), as.Date("2023-01-13")), 10L)
})

test_that("countWorkDays nonworkdays parameter changes which days are excluded", {
  # Mon 2023-01-02 to Fri 2023-01-06, Saturday is still included
  res <- countWorkDays(as.Date("2023-01-02"), as.Date("2023-01-07"),
                       nonworkdays = "Sun")
  # Mon-Sat = 6 days
  expect_equal(res, 6L)
})
