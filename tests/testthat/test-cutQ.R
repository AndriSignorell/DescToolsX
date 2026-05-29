test_that("cutQ returns a factor", {
  x <- rnorm(100)
  expect_s3_class(cutQ(x), "factor")
})

test_that("cutQ default produces 4 levels (quartiles)", {
  set.seed(1)
  x <- rnorm(200)
  q <- cutQ(x)
  expect_equal(nlevels(q), 4L)
})

test_that("cutQ default labels are Q1, Q2, Q3, Q4", {
  set.seed(1)
  x <- rnorm(200)
  expect_equal(levels(cutQ(x)), paste0("Q", 1:4))
})

test_that("cutQ breaks = 10 produces deciles (up to 10 levels)", {
  set.seed(2)
  x <- rnorm(1000)
  q <- cutQ(x, breaks = 10)
  expect_lte(nlevels(q), 10L)
})

test_that("cutQ total count equals length of x (no values dropped)", {
  set.seed(3)
  x <- rnorm(500)
  q <- cutQ(x)
  expect_equal(sum(table(q)), length(x))
})

test_that("cutQ handles tied quantiles without error", {
  # Rounding creates many ties → fewer than 10 groups expected
  set.seed(4)
  x <- round(rnorm(200))
  expect_s3_class(cutQ(x, breaks = 10), "factor")
})

test_that("cutQ labels = FALSE returns integer codes", {
  set.seed(5)
  x <- rnorm(100)
  q <- cutQ(x, labels = FALSE)
  expect_type(q, "integer")
})

test_that("cutQ custom labels are used when provided", {
  set.seed(6)
  x <- rnorm(100)
  lbl <- c("low","mid-low","mid-high","high")
  q   <- cutQ(x, labels = lbl)
  expect_equal(levels(q), lbl)
})

test_that("cutQ na.rm = TRUE removes NAs before computing quantiles", {
  set.seed(7)
  x <- c(rnorm(96), NA, NA, NA, NA)
  expect_s3_class(cutQ(x, na.rm = TRUE), "factor")
})

test_that("cutQ result has roughly equal group sizes for continuous data", {
  set.seed(8)
  x <- rnorm(400)
  q <- cutQ(x)
  counts <- table(q)
  # each quartile should be around 100; allow +-5% tolerance
  expect_true(all(counts >= 90 & counts <= 110))
})
