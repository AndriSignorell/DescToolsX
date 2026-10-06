
test_that("unweighted quantileX is stats::quantile", {

  set.seed(1)
  x <- rnorm(50)

  for (ty in 1:9)
    expect_equal(quantileX(x, type = ty), quantile(x, type = ty),
                 label = paste("type", ty))

  expect_equal(quantileX(x, probs = c(0.1, 0.9), names = FALSE),
               quantile(x, probs = c(0.1, 0.9), names = FALSE))
})


test_that("type 2 depends only on the ratios of the weights", {

  x <- c(3.7, 3.3, 3.5, 2.8)
  w <- c(5, 5, 4, 1)

  a <- quantileX(x, weights = w,      probs = c(0.25, 0.75), type = 2)
  b <- quantileX(x, weights = w / 15, probs = c(0.25, 0.75), type = 2)
  d <- quantileX(x, weights = w * 7,  probs = c(0.25, 0.75), type = 2)

  expect_equal(a, b)
  expect_equal(a, d)
})


test_that("weighted type 2 with equal weights is the unweighted type 2", {

  set.seed(3)
  x <- rnorm(20)
  p <- c(0, 0.05, 0.1, 0.25, 0.3, 0.5, 0.75, 0.95, 1)

  # scale must not matter, including normalized weights whose cumsum()
  # carries rounding error (the old exact rw == p comparison)
  for (w in list(rep(1, 20), rep(0.05, 20), rep(1/3, 20), rep(7, 20)))
    expect_equal(quantileX(x, weights = w, probs = p, type = 2),
                 quantile(x, probs = p, type = 2))

  # the case that exposed the old label: R's type 5 differs here
  y <- c(2.8, 3.3, 3.5, 3.7)
  expect_equal(unname(quantileX(y, weights = rep(1, 4), probs = 0.3,
                                type = 2)), 3.3)
  expect_equal(unname(quantile(y, 0.3, type = 5)), 3.15)
})


test_that("weighted type 5 is a silent alias of type 2", {

  x <- c(3.7, 3.3, 3.5, 2.8)
  w <- c(5, 5, 4, 1)

  expect_silent(q5 <- quantileX(x, weights = w, type = 5))
  expect_identical(q5, quantileX(x, weights = w, type = 2))
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
  # type 2, which depends only on the ratios of the weights. That is what
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
  expect_equal(quantileX(x, weights = w, na.rm = TRUE, type = 2),
               quantileX(c(1, 2, 4), weights = c(1, 1, 1), type = 2))
})


test_that("unweighted missing values give NA as well, not an error", {

  x <- c(1, 2, NA, 4)

  # stats::quantile() stops here; quantileX() behaves like mean()
  q <- quantileX(x)
  expect_length(q, 5L)
  expect_true(all(is.na(q)))
  expect_type(q, "double")
  expect_named(q, c("0%", "25%", "50%", "75%", "100%"))

  expect_null(names(quantileX(x, names = FALSE)))
  expect_equal(quantileX(x, na.rm = TRUE), quantile(x, na.rm = TRUE))
})


test_that("checks see the data after NA removal", {

  # the only positive weight sits on the NA: nothing left to weigh
  x <- c(1, 2, NA)
  expect_warning(q <- quantileX(x, weights = c(0, 0, 5), na.rm = TRUE,
                                type = 2), "zero")
  expect_true(all(is.na(q)))
  expect_warning(quantileX(x, weights = c(0, 0, 5), na.rm = TRUE, type = 7),
                 "zero")

  # all-NA x with na.rm: NA, not a misleading "sum must be at least 2"
  q <- quantileX(c(NA_real_, NA_real_), weights = c(1, 1), na.rm = TRUE,
                 probs = c(0.25, 0.75), type = 7)
  expect_equal(unname(q), c(NA_real_, NA_real_))
})


test_that("NA in probs gives NA at that position", {

  x <- c(3.7, 3.3, 3.5, 2.8)
  w <- c(5, 5, 4, 1)
  p <- c(0.5, NA, 0.25)

  for (ty in c(2, 7)) {
    q <- quantileX(x, weights = w, probs = p, type = ty)
    expect_length(q, 3L)
    expect_true(is.na(q[2L]))
    expect_false(anyNA(q[-2L]))
    expect_equal(names(q), names(quantile(x, probs = p)))
  }
  expect_equal(quantileX(x, probs = p), quantile(x, probs = p))
})


test_that("empty probs give an empty double", {

  x <- c(3.7, 3.3, 3.5, 2.8)
  for (ty in c(2, 7))
    expect_identical(unname(quantileX(x, weights = c(5, 5, 4, 1),
                                      probs = numeric(0), type = ty)),
                     numeric(0))
})


test_that("invalid input is refused clearly", {

  x <- c(1, 2, 3, 4)
  w <- c(1, 1, 1, 1)

  # negative weights make cumsum() non-monotonic, which both branches read
  # as an increasing index
  expect_error(quantileX(x, weights = c(1, -1, 1, 1)), "not be negative")

  # was: a warning plus qs <- NA, which then failed on names<-
  expect_error(quantileX(x, weights = w, type = 3), "not implemented")
  expect_error(quantileX(x, weights = w, type = 10), "between 1 and 9")
  expect_error(quantileX(x, weights = w, type = 1), "not implemented")

  expect_error(quantileX(x, weights = c(1, 1)), "same length")
  expect_error(quantileX(x, weights = w, probs = c(-0.1, 0.5)), "\\[0,1\\]")
})


test_that("names are attached consistently on every path", {

  x <- c(1, 2, 3, 4)
  w <- c(2, 2, 2, 2)

  expect_named(quantileX(x, weights = w, type = 2))
  expect_named(quantileX(x, weights = w, type = 7))
  expect_null(names(quantileX(x, weights = w, type = 2, names = FALSE)))
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

  expect_equal(unname(quantileX(x, weights = w, probs = 0.5, type = 2)),
               unname(quantileX(c(2, 7, 9), weights = c(3, 1, 2),
                                probs = 0.5, type = 2)))
})


test_that("NA and NaN weights are missing data, not an error", {

  x <- c(3.7, 3.3, 3.5, 2.8, 4.1)
  w <- c(5, 5, 4, 1, NA)

  for (bad in list(NA_real_, NaN)) {
    w[5] <- bad
    for (ty in c(2, 7)) {
      q <- quantileX(x, weights = w, type = ty)
      expect_true(all(is.na(q)))
      expect_named(q)

      # na.rm drops the pair
      expect_equal(quantileX(x, weights = w, na.rm = TRUE, type = ty),
                   quantileX(x[-5], weights = w[-5], type = ty))
    }
  }

  # NaN in x as well
  expect_true(all(is.na(quantileX(c(1, NaN, 3)))))
  expect_true(all(is.na(quantileX(c(1, NaN, 3), weights = c(1, 1, 1)))))
})


test_that("invalid arguments are not masked by missing data", {

  x <- c(1, NA, 3)
  w <- c(1, 1, 1)

  expect_error(quantileX(x, probs = 1.5), "\\[0,1\\]")
  expect_error(quantileX(x, weights = w, probs = 1.5), "\\[0,1\\]")
  expect_error(quantileX(x, weights = c(1, NA, -1)), "not be negative")
  expect_error(quantileX(x, weights = c(1, NA, Inf)), "finite")
  expect_error(quantileX(x, weights = w, type = 3), "not implemented")
  expect_error(quantileX(x, type = 10), "between 1 and 9")
  expect_error(quantileX(x, weights = c(1, 1)), "same length")
})


test_that("nothing left after na.rm gives NA without warning", {

  for (ty in c(2, 7)) {
    expect_silent(q <- quantileX(c(NA, NaN), weights = c(1, 1),
                                 na.rm = TRUE, type = ty))
    expect_true(all(is.na(q)))
    expect_silent(q <- quantileX(c(1, 2), weights = c(NA, NA),
                                 na.rm = TRUE, type = ty))
    expect_true(all(is.na(q)))
  }
  expect_true(all(is.na(quantileX(c(NA_real_, NA_real_), na.rm = TRUE))))
})


test_that("an NA in probs only affects its own position", {

  x <- c(3.7, 3.3, 3.5, 2.8)
  w <- c(5, 5, 4, 1)
  p <- c(0.25, NA, 0.75)

  for (ty in c(2, 7))
    expect_equal(unname(quantileX(x, weights = w, probs = p, type = ty)[-2]),
                 unname(quantileX(x, weights = w, probs = p[-2], type = ty)))
})
