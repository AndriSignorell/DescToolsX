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



test_that("countWorkDays counts both endpoints and handles reversed dates", {
  
  # 2019-01-07 is a Monday
  mon <- as.Date("2019-01-07")
  expect_equal(countWorkDays(mon, mon), 1L)
  expect_equal(countWorkDays(mon, mon + 4), 5L)      # Mon..Fri
  expect_equal(countWorkDays(mon, mon + 6), 5L)      # full week
  expect_equal(countWorkDays(as.Date("2019-01-05"),
                             as.Date("2019-01-05")), 0L)  # Saturday
  
  # reversed pair used to abort with "wrong sign in 'by' argument"
  expect_equal(countWorkDays(mon + 4, mon), 0L)
})


test_that("countWorkDays subtracts holidays once and validates nonworkdays", {
  
  mon <- as.Date("2019-01-07")
  
  expect_equal(countWorkDays(mon, mon + 4,
                             holiday = c("2019-01-08", "2019-01-09")), 3L)
  # duplicated holidays count once
  expect_equal(countWorkDays(mon, mon + 4,
                             holiday = rep("2019-01-08", 3)), 4L)
  # a holiday on a weekend changes nothing
  expect_equal(countWorkDays(mon, mon + 6, holiday = "2019-01-12"), 5L)
  
  expect_error(countWorkDays(mon, mon + 4, nonworkdays = "Sunday"), "subset")
})

