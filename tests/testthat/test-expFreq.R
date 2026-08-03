test_that("expFreq returns an array of the same dimensions as input", {
  expect_equal(dim(expFreq(Titanic)), dim(Titanic))
})

test_that("expFreq absolute frequencies sum to the same total as input", {
  expect_equal(sum(expFreq(Titanic)), sum(Titanic))
})

test_that("expFreq relative frequencies sum to 1", {
  rel <- expFreq(Titanic, freq = "rel")
  expect_equal(sum(rel), 1, tolerance = 1e-10)
})

test_that("expFreq all values are non-negative", {
  ef <- expFreq(Titanic)
  expect_true(all(ef >= 0))
})

test_that("expFreq preserves dimnames of the input table", {
  ef <- expFreq(Titanic)
  expect_equal(dimnames(ef), dimnames(Titanic))
})

test_that("expFreq for a 2-way table matches chisq.test expected values", {
  tab <- as.table(matrix(c(20, 30, 40, 10), nrow = 2))
  ef  <- expFreq(tab)
  chi <- chisq.test(tab, correct = FALSE)
  expect_equal(as.numeric(ef), as.numeric(chi$expected), tolerance = 1e-8)
})

test_that("expFreq works for 3-dimensional tables", {
  ef <- expFreq(UCBAdmissions)
  expect_equal(dim(ef), dim(UCBAdmissions))
  expect_equal(sum(ef), sum(UCBAdmissions), tolerance = 1e-8)
})

test_that("expFreq stops when passed a non-array (e.g. data frame)", {
  expect_error(expFreq(data.frame(a = 1:3, b = 1:3)))
})

test_that("expFreq freq = 'r' is accepted as abbreviation for 'rel'", {
  rel_full  <- expFreq(Titanic, freq = "rel")
  rel_abbr  <- expFreq(Titanic, freq = "r")
  expect_equal(rel_full, rel_abbr)
})




test_that("expFreq keeps the table class and reproduces chisq.test", {
  
  tab <- apply(HairEyeColor, c(1, 2), sum)
  e <- expFreq(as.table(tab))
  
  expect_s3_class(e, "table")
  expect_equal(unname(as.matrix(e)),
               unname(suppressWarnings(chisq.test(tab)$expected)),
               tolerance = 1e-10)
  
  expect_equal(sum(expFreq(Titanic, freq = "rel")), 1)
})
