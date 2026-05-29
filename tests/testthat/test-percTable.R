.tab <- as.table(apply(HairEyeColor, c(1,2), sum))

test_that("percTable returns an object of class 'PercTable'", {
  expect_s3_class(percTable(.tab), "PercTable")
})

test_that("percTable contains 'freq' by default", {
  res <- percTable(.tab)
  expect_true("freq" %in% names(res))
})

test_that("percTable freq = FALSE omits frequency table", {
  res <- percTable(.tab, freq = FALSE, prop = "rows")
  expect_false("freq" %in% names(res))
})

test_that("percTable prop = 'rows' contains p.row", {
  res <- percTable(.tab, prop = "rows")
  expect_true("p.row" %in% names(res))
})

test_that("percTable prop = 'cols' contains p.col", {
  res <- percTable(.tab, prop = "cols")
  expect_true("p.col" %in% names(res))
})

test_that("percTable prop = 'total' contains perc", {
  res <- percTable(.tab, prop = "total")
  expect_true("perc" %in% names(res))
})

test_that("percTable expected = TRUE adds expected frequencies", {
  res <- percTable(.tab, prop = "none", expected = TRUE)
  expect_true("expected" %in% names(res))
})

test_that("percTable print method works without error", {
  expect_output(print(percTable(.tab, col.vars = 2)))
})

test_that("percTable formula interface works", {
  res <- percTable(driver ~ area, data = d.pizza)
  expect_s3_class(res, "PercTable")
})

test_that("percTable vector interface works", {
  res <- percTable(x = d.pizza$driver, y = d.pizza$area)
  expect_s3_class(res, "PercTable")
})
