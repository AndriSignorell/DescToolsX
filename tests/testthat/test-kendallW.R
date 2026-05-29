
.anxiety <- data.frame(
  rater1=c(3,3,3,4,5,5,2,3,5,2,2,6,1,5,2,2,1,2,4,3),
  rater2=c(3,6,4,6,2,4,2,4,3,3,2,3,3,3,2,2,1,3,3,4),
  rater3=c(2,1,4,4,3,2,1,6,1,1,1,2,3,3,1,1,3,3,2,2)
)

test_that("kendallW returns a single numeric by default", {
  w <- kendallW(.anxiety)
  expect_length(w, 1); expect_true(is.numeric(w))
})

test_that("kendallW result is in [0, 1]", {
  w <- kendallW(.anxiety)
  expect_gte(w, 0); expect_lte(w, 1)
})

test_that("kendallW perfect concordance gives W = 1", {
  x <- data.frame(A = 1:5, B = 1:5, C = 1:5)
  expect_equal(kendallW(x), 1, tolerance = 1e-6)
})

test_that("kendallW correct = TRUE gives different result when ties present", {
  # .anxiety has ties
  w_raw  <- suppressWarnings(kendallW(.anxiety, correct = FALSE))
  w_corr <- kendallW(.anxiety, correct = TRUE)
  expect_false(isTRUE(all.equal(w_raw, w_corr)))
})

test_that("kendallW warns when ties present and correct = FALSE", {
  expect_warning(kendallW(.anxiety, correct = FALSE), "ties")
})

test_that("kendallW test = TRUE returns an htest object", {
  res <- kendallW(.anxiety, test = TRUE)
  expect_s3_class(res, "htest")
})

test_that("kendallW test = TRUE has estimate, statistic, p.value", {
  res <- kendallW(.anxiety, test = TRUE)
  expect_named(res, c("estimate","parameter","statistic","p.value",
                      "alternative","method","data.name"))
})

test_that("kendallW is equivalent to friedman.test for the doc example", {
  # The equivalence holds when kendallW receives the TRANSPOSE of the
  # friedman.test matrix (kendallW: rows=subjects, cols=raters;
  # friedman.test: rows=blocks, cols=treatments)
  d.att <- data.frame(
    id        = c(4L, 21L, 11L),
    airfare   = c(5L, 1L, 4L),
    climate   = c(6L, 7L, 5L),
    season    = c(7L, 6L, 1L),
    people    = c(1L, 2L, 3L),
    program   = c(2L, 3L, 2L),
    publicity = c(4L, 5L, 7L),
    present   = c(3L, 4L, 6L),
    interest  = c(8L, 8L, 8L)
  )
  kw  <- kendallW(t(d.att[, -1]), test = TRUE)
  frd <- friedman.test(y = as.matrix(d.att[, -1]), groups = d.att$id)
  expect_equal(unname(kw$statistic), unname(frd$statistic), tolerance = 1e-6)
})

test_that("kendallW handles NA via generalized formula", {
  dat_na <- .anxiety
  dat_na[1, 1] <- NA
  w <- kendallW(dat_na)
  expect_gte(w, 0); expect_lte(w, 1)
})

