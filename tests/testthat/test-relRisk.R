
test_that("relRisk returns correct point estimate", {
  
  x <- matrix(
    c(78, 50,
      1422, 950),
    nrow = 2
  )
  
  rr <- relRisk(x)
  
  expect_equal(
    rr,
    (78 / (78 + 1422)) / (50 / (50 + 950))
  )
  
})



test_that("relRisk returns named confidence interval output", {
  
  x <- matrix(
    c(78, 50,
      1422, 950),
    nrow = 2
  )
  
  rr <- relRisk(
    x,
    conf.level = 0.95
  )
  
  expect_named(
    rr,
    c("estimate", "lci", "uci")
  )
  
  expect_true(is.numeric(rr))
  
  expect_equal(length(rr), 3L)
  
})



test_that("relRisk supports all methods", {
  
  x <- matrix(
    c(9, 20,
      41, 29),
    nrow = 2
  )
  
  expect_no_error(
    relRisk(
      x,
      conf.level = 0.95,
      method = "score"
    )
  )
  
  expect_no_error(
    relRisk(
      x,
      conf.level = 0.95,
      method = "wald"
    )
  )
  
  expect_no_error(
    relRisk(
      x,
      conf.level = 0.95,
      method = "use-or"
    )
  )
  
})



test_that("relRisk validates input type", {
  
  expect_error(
    relRisk("foo"),
    "Argument 'x' must be numeric."
  )
  
})



test_that("relRisk rejects missing values", {
  
  x <- matrix(
    c(1, 2,
      NA, 4),
    nrow = 2
  )
  
  expect_error(
    relRisk(x),
    "must not contain missing values"
  )
  
})



test_that("relRisk rejects non-2x2 matrices", {
  
  x <- matrix(1:9, nrow = 3)
  
  expect_error(
    relRisk(x),
    "must be a 2x2 matrix"
  )
  
})



test_that("relRisk rejects negative counts", {
  
  x <- matrix(
    c(1, -1,
      2, 3),
    nrow = 2
  )
  
  expect_error(
    relRisk(x),
    "must contain non-negative counts"
  )
  
})



test_that("relRisk rejects non-integer counts", {
  
  x <- matrix(
    c(1.5, 2,
      3, 4),
    nrow = 2
  )
  
  expect_error(
    relRisk(x),
    "must contain integer counts"
  )
  
})



test_that("relRisk rejects zero row totals", {
  
  x <- matrix(
    c(0, 0,
      1, 2),
    nrow = 2,
    byrow = TRUE
  )
  
  expect_error(
    relRisk(x),
    "must contain positive totals"
  )
  
})



test_that("relRisk handles zero-event boundary case", {
  
  x <- matrix(
    c(0, 0,
      10, 20),
    nrow = 2
  )
  
  rr <- relRisk(
    x,
    conf.level = 0.95
  )
  
  expect_identical(rr[["lci"]], 0)
  expect_true(is.infinite(rr["uci"]))
  
})



test_that("relRisk handles complete-event boundary case", {
  
  x <- matrix(
    c(10, 10,
      0, 0),
    nrow = 2
  )
  
  rr <- relRisk(
    x,
    conf.level = 0.95
  )
  
  expect_true(rr["lci"] > 0)
  expect_true(rr["uci"] > 1)
  
})



test_that("relRisk accepts x and y input", {
  
  x <- c(1, 1, 0, 0, 1, 0)
  y <- c(1, 0, 1, 0, 1, 0)
  
  expect_no_error(
    relRisk(
      x,
      y,
      conf.level = 0.95
    )
  )
  
})

