

test_that("medianX.Freq works when the first class holds the median", {
  
  # x[mi - 1, "cumfreq"] selected zero rows for mi == 1, so the whole
  # expression collapsed to numeric(0)
  ft <- freq(as.table(c(80, 10, 5, 5)))
  res <- medianX(ft, breaks = c(0, 4000, 6000, 8000, 10000))
  
  expect_length(res, 1L)
  expect_false(is.na(res))
  expect_gte(res, 0); expect_lte(res, 4000)
  
  # and the ordinary case still matches the documented example
  ft2 <- freq(as.table(c(20, 42, 31, 12)))
  expect_length(medianX(ft2, breaks = c(0, 4000, 6000, 8000, 10000)), 1L)
})


test_that("the weighted median is scale invariant", {
  
  x <- c(3.7, 3.3, 3.5, 2.8)
  w <- c(5, 5, 4, 1)
  
  # medianX passed the weights to quantileX with its type-7 default,
  # which reads them as replication counts; normalized weights then
  # collapsed every quantile onto max(x)
  expect_equal(medianX(x, weights = w), medianX(x, weights = w / 15))
  expect_equal(medianX(x, weights = w), medianX(x, weights = w * 7))
  
  # and counts still agree with the replicated sample
  z <- c(2, 5, 9)
  expect_equal(medianX(z, weights = c(3, 1, 2)),
               medianX(rep(z, c(3, 1, 2))))
})


test_that("medianX.factor refuses an unordered factor", {
  
  f <- factor(c("a", "b", "c", "b"))
  expect_error(medianX(f), "unordered")
  
  o <- factor(c("lo", "mid", "hi", "mid"),
              levels = c("lo", "mid", "hi"), ordered = TRUE)
  expect_equal(as.character(medianX(o)), "mid")
})

