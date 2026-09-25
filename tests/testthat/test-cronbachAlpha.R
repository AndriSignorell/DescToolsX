# ---- shared test data ----
set.seed(1234)
.items_good <- data.frame(
  i1 = c(1,2,3,4,5,4,3,2,1,2,3,4,5,4,3,2,1,2,3,4),
  i2 = c(1,2,3,4,5,4,3,2,1,2,3,4,5,4,3,2,1,2,3,4) + rnorm(20, 0, 0.5),
  i3 = c(1,2,3,4,5,4,3,2,1,2,3,4,5,4,3,2,1,2,3,4) + rnorm(20, 0, 0.5),
  i4 = c(1,2,3,4,5,4,3,2,1,2,3,4,5,4,3,2,1,2,3,4) + rnorm(20, 0, 0.5)
)

.items_random <- data.frame(
  i1 = sample(1:5, 20, replace = TRUE),
  i2 = sample(1:5, 20, replace = TRUE),
  i3 = sample(1:5, 20, replace = TRUE)
)

test_that("cronbachAlpha returns a single numeric by default", {
  a <- cronbachAlpha(.items_good)
  expect_length(a, 1)
  expect_true(is.numeric(a))
})

test_that("cronbachAlpha is high for strongly correlated items", {
  a <- cronbachAlpha(.items_good)
  expect_gt(a, 0.8)
})

test_that("cronbachAlpha is lower for uncorrelated items", {
  a_good   <- cronbachAlpha(.items_good)
  a_random <- cronbachAlpha(.items_random)
  expect_gt(a_good, a_random)
})

test_that("cronbachAlpha result is <= 1", {
  a <- cronbachAlpha(.items_good)
  expect_lte(a, 1)
})

test_that("cronbachAlpha conf.level returns a 3-element named vector", {
  a <- cronbachAlpha(.items_good, conf.level = 0.95)
  expect_length(a, 3)
  expect_named(a, c("est","lci","uci"))
})

test_that("cronbachAlpha CI: lci < estimate < uci", {
  a <- cronbachAlpha(.items_good, conf.level = 0.95)
  expect_lt(a["lci"], a["est"])
  expect_gt(a["uci"], a["est"])
})

test_that("cronbachAlpha returnConditional = TRUE returns a list", {
  res <- cronbachAlpha(.items_good, returnConditional = TRUE)
  expect_type(res, "list")
  expect_named(res, c("unconditional","conditional"))
})

test_that("cronbachAlpha returnConditional = TRUE conditional has ncol(x) rows", {
  res <- cronbachAlpha(.items_good, returnConditional = TRUE)
  expect_equal(nrow(res$conditional), ncol(.items_good))
})

test_that("cronbachAlpha na.rm = TRUE handles missing values", {
  items_na <- .items_good
  items_na[1, 1] <- NA
  a <- cronbachAlpha(items_na, na.rm = TRUE)
  expect_true(is.numeric(a))
})

test_that("cronbachAlpha works with a matrix input", {
  m <- as.matrix(.items_good)
  expect_true(is.numeric(cronbachAlpha(m)))
})



test_that("cronbachAlpha keeps its shape for missing input", {
  
  set.seed(9)
  d <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10))
  d$a[1] <- NA
  
  res <- cronbachAlpha(d, conf.level = 0.95)
  expect_named(res, c("est", "lci", "uci"))
  expect_true(all(is.na(res)))
  
  resCond <- cronbachAlpha(d, returnConditional = TRUE, conf.level = 0.95)
  expect_named(resCond, c("unconditional", "conditional"))
})


test_that("cronbachAlpha closes the bounded side at 1", {
  
  set.seed(10)
  d <- as.data.frame(matrix(rnorm(60), ncol = 3))
  
  left <- cronbachAlpha(d, conf.level = 0.95, sides = "left")
  right <- cronbachAlpha(d, conf.level = 0.95, sides = "right")
  
  expect_equal(unname(left[["uci"]]), 1)      # alpha <= 1
  expect_identical(unname(right[["lci"]]), -Inf)  # unbounded below
})


# Additional branch coverage and reference checks
test_that("cronbachAlpha validates all scalar arguments and data dimensions", {
  x <- matrix(c(1, 2, 4, 3, 2, 4, 5, 6, 3, 4, 6, 8), 4)
  expect_error(cronbachAlpha(1:4), "matrix or a data frame")
  expect_error(cronbachAlpha(matrix(letters[1:6], 3)), "numeric values")
  for (bad in list(1, NA, logical(), c(TRUE, FALSE))) {
    expect_error(cronbachAlpha(x, returnConditional = bad), "returnConditional")
    expect_error(cronbachAlpha(x, na.rm = bad), "na.rm")
  }
  for (bad in list("0.95", numeric(), c(0.9, 0.95), NaN, Inf, 0, 1, TRUE))
    expect_error(cronbachAlpha(x, conf.level = bad), "conf.level")
  expect_error(cronbachAlpha(x, sides = "invalid"), "arg")
  expect_error(cronbachAlpha(matrix(1:4, ncol = 1)), "at least 2 items")
  expect_error(cronbachAlpha(matrix(1:3, nrow = 1)), "at least 2 subjects")
  x[1, 1] <- Inf
  expect_error(cronbachAlpha(x), "infinite values")
})

test_that("cronbachAlpha handles undefined totals and missing conditional results", {
  x <- cbind(1:4, 4:1) # Each subject has the same total score.
  expect_true(is.na(cronbachAlpha(x)))
  expect_true(all(is.na(cronbachAlpha(x, conf.level = 0.95))))
  x[1, 1] <- NA_real_
  res <- cronbachAlpha(x, returnConditional = TRUE)
  expect_true(is.na(res$unconditional))
  expect_null(res$conditional)
  res <- cronbachAlpha(x, conf.level = 0.95, returnConditional = TRUE)
  expect_named(res$unconditional, c("est", "lci", "uci"))
  expect_true(all(is.na(res$unconditional)))
  expect_null(res$conditional)
})

test_that("cronbachAlpha conditional rows agree with independently deleted items", {
  x <- matrix(c(1, 2, 4, 3, 2, 4, 5, 6, 3, 4, 6, 8), 4)
  res <- cronbachAlpha(x, conf.level = 0.9, returnConditional = TRUE)
  expect_identical(res$conditional$item, as.character(1:3))
  for (i in 1:3) {
    expect_equal(as.numeric(res$conditional[i, -1]),
                 unname(cronbachAlpha(x[, -i], conf.level = 0.9)))
  }
  expect_null(cronbachAlpha(x[, 1:2], returnConditional = TRUE)$conditional)
  x[1, 1] <- NA
  expect_equal(cronbachAlpha(x, na.rm = TRUE), cronbachAlpha(x[-1, ]))
})

test_that("cronbachAlpha one-sided finite bounds agree with F quantiles", {
  x <- matrix(c(1, 2, 4, 3, 2, 4, 5, 6, 3, 4, 6, 8), 4)
  a <- unname(cronbachAlpha(x))
  left <- cronbachAlpha(x, conf.level = 0.95, sides = "left")
  right <- cronbachAlpha(x, conf.level = 0.95, sides = "right")
  expect_equal(left[["lci"]], 1 - (1 - a) * qf(0.95, 3, 6))
  expect_equal(right[["uci"]], 1 - (1 - a) * qf(0.05, 3, 6))
})
