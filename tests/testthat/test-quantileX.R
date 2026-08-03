
test_that("unweighted quantileX is stats::quantile", {

  set.seed(1)
  x <- rnorm(50)

  for (ty in 1:9)
    expect_equal(quantileX(x, type = ty), quantile(x, type = ty),
                 label = paste("type", ty))

  expect_equal(quantileX(x, probs = c(0.1, 0.9), names = FALSE),
               quantile(x, probs = c(0.1, 0.9), names = FALSE))
})


test_that("type 5 depends only on the ratios of the weights", {

  x <- c(3.7, 3.3, 3.5, 2.8)
  w <- c(5, 5, 4, 1)

  a <- quantileX(x, weights = w,      probs = c(0.25, 0.75), type = 5)
  b <- quantileX(x, weights = w / 15, probs = c(0.25, 0.75), type = 5)
  d <- quantileX(x, weights = w * 7,  probs = c(0.25, 0.75), type = 5)

  expect_equal(a, b)
  expect_equal(a, d)
})


test_that("type 7 refuses normalized weights instead of collapsing", {

  x <- c(3.7, 3.3, 3.5, 2.8)
  w <- c(5, 5, 4, 1)

  # With sum(weights) == 1 the formula ord = 1 + (sumW - 1) * probs gives 1
  # for EVERY prob, so all quantiles used to come back as max(x) and the
  # IQR as 0 - which is exactly what ?iqrX demonstrated.
  expect_error(quantileX(x, weights = w / 15, probs = c(0.25, 0.75), type = 7),
               "at least 2")

  # on the count scale it works
  q <- quantileX(x, weights = w, probs = c(0.25, 0.75), type = 7)
  expect_equal(unname(q), c(3.3, 3.7))
})


test_that("iqrX picks the quantile type that suits its branch", {

  x <- c(3.7, 3.3, 3.5, 2.8)
  w <- c(5, 5, 4, 1)

  # iqrX no longer hits the type-7 guard: its weighted branch defaults to
  # type 5, which depends only on the ratios of the weights. That is what
  # makes its own documented example - w <- c(5, 5, 4, 1)/15 - work.
  expect_no_error(iqrX(x, weights = w / 15))

  expect_equal(iqrX(x, weights = w), iqrX(x, weights = w / 15))
  expect_equal(iqrX(x, weights = w), iqrX(x, weights = w * 7))

  # an explicit type is still honoured, guard included
  expect_error(iqrX(x, weights = w / 15, type = 7), "at least 2")
  expect_equal(iqrX(x, weights = w, type = 7), 0.4)

  # and the unweighted branch is IQR()
  expect_equal(iqrX(x), IQR(x))
  expect_equal(iqrX(x, type = 6), IQR(x, type = 6))
})


test_that("integer weights reproduce the replicated sample for type 7", {

  x <- c(2, 5, 9)
  w <- c(3, 1, 2)
  rep_x <- rep(x, w)

  # frequency weights are replication counts, so the weighted result must
  # match the expanded vector
  expect_equal(unname(quantileX(x, weights = w, probs = c(0.25, 0.5, 0.75),
                                type = 7)),
               unname(quantile(rep_x, probs = c(0.25, 0.5, 0.75), type = 7)))
})


test_that("degenerate weights give NA, not a fabricated zero", {

  x <- c(3.7, 3.3, 3.5, 2.8)

  # was rep.int(0, length(probs)) - a number that looks like a quantile
  expect_warning(q <- quantileX(x, weights = rep(0, 4)), "zero")
  expect_true(all(is.na(q)))
  expect_type(q, "double")
  expect_named(q)
})


test_that("missing values return named NA of the documented length", {

  x <- c(1, 2, NA, 4)
  w <- c(1, 1, 1, 1)

  q <- quantileX(x, weights = w)
  expect_length(q, 5L)
  expect_true(all(is.na(q)))
  expect_type(q, "double")
  expect_named(q, c("0%", "25%", "50%", "75%", "100%"))

  # na.rm drops the pair and computes on the rest
  expect_equal(quantileX(x, weights = w, na.rm = TRUE, type = 5),
               quantileX(c(1, 2, 4), weights = c(1, 1, 1), type = 5))
})


test_that("invalid input is refused clearly", {

  x <- c(1, 2, 3, 4)
  w <- c(1, 1, 1, 1)

  # negative weights make cumsum() non-monotonic, which both branches read
  # as an increasing index
  expect_error(quantileX(x, weights = c(1, -1, 1, 1)), "not be negative")

  # was: a warning plus qs <- NA, which then failed on names<-
  expect_error(quantileX(x, weights = w, type = 3), "not implemented")
  expect_error(quantileX(x, weights = w, type = 1), "not implemented")

  expect_error(quantileX(x, weights = c(1, 1)), "same length")
  expect_error(quantileX(x, weights = w, probs = c(-0.1, 0.5)), "\\[0,1\\]")
})


test_that("names are attached consistently on every path", {

  x <- c(1, 2, 3, 4)
  w <- c(2, 2, 2, 2)

  expect_named(quantileX(x, weights = w, type = 5))
  expect_named(quantileX(x, weights = w, type = 7))
  expect_null(names(quantileX(x, weights = w, type = 5, names = FALSE)))
})


test_that("zero weights are dropped rather than tying the cumulative sum", {

  x <- c(2, 5, 7, 9)
  w <- c(3, 0, 1, 2)

  # a zero weight repeats a value in cumsum(weights); approx() then
  # collapses the tie and warns about it
  expect_silent(q <- quantileX(x, weights = w, probs = c(0.25, 0.75),
                               type = 7))

  # and the result must equal the one with that observation left out
  expect_equal(q, quantileX(c(2, 7, 9), weights = c(3, 1, 2),
                            probs = c(0.25, 0.75), type = 7))

  expect_equal(unname(quantileX(x, weights = w, probs = 0.5, type = 5)),
               unname(quantileX(c(2, 7, 9), weights = c(3, 1, 2),
                                probs = 0.5, type = 5)))
})
