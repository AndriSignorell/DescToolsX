test_that("scaleX returns a matrix", {
  x <- rnorm(20)
  expect_true(is.matrix(scaleX(x)))
})

test_that("scaleX centered result has median near 0", {
  set.seed(1)
  x <- rnorm(200, mean = 5, sd = 2)
  res <- scaleX(x)
  expect_lt(abs(median(res)), 0.1)
})

test_that("scaleX scaled result has mad near 1", {
  set.seed(2)
  x <- rnorm(200, mean = 5, sd = 2)
  res <- scaleX(x)
  expect_equal(mad(res), 1, tolerance = 0.05)
})

test_that("scaleX center = FALSE skips centering", {
  x <- matrix(c(1,2,3,4,5,6), ncol=2)
  res <- scaleX(x, center = FALSE, scale = FALSE)
  expect_equal(res[,1], x[,1])
})

test_that("scaleX scale = FALSE skips scaling", {
  x <- matrix(c(1,2,3,10,20,30), ncol=2)
  res <- scaleX(x, scale = FALSE)
  # column should have same spread but shifted to median = 0
  expect_equal(median(res[,1]), 0, tolerance = 1e-10)
  expect_equal(sd(res[,1]), sd(x[,1]), tolerance = 1e-10)
})

test_that("scaleX result has 'scaled:center' attribute", {
  x <- rnorm(30, mean=5)
  res <- scaleX(x)
  expect_false(is.null(attr(res, "scaled:center")))
})

test_that("scaleX result has 'scaled:scale' attribute", {
  x <- rnorm(30, sd=3)
  res <- scaleX(x)
  expect_false(is.null(attr(res, "scaled:scale")))
})

test_that("scaleX works column-wise for matrices", {
  x <- matrix(c(rnorm(20, mean=0), rnorm(20, mean=10)), ncol=2)
  res <- scaleX(x)
  expect_lt(abs(median(res[,1])), 0.2)
  expect_lt(abs(median(res[,2])), 0.2)
})


test_that("scaleX centers and scales by median and MAD", {
  
  x <- cbind(a = c(1, 2, 3, 4, 100), b = c(10, 20, 30, 40, 50))
  z <- scaleX(x)
  
  expect_equal(attr(z, "scaled:center"), apply(x, 2, median))
  expect_equal(attr(z, "scaled:scale"), apply(x, 2, mad))
  expect_equal(as.vector(z[, "b"]), (x[, "b"] - 30) / mad(x[, "b"]))
  
})


test_that("both scaled: attributes survive when center and scale are used", {
  
  # regression test: chaining two scale() calls dropped "scaled:center"
  z <- scaleX(cbind(a = 1:5, b = 6:10))
  
  expect_false(is.null(attr(z, "scaled:center")))
  expect_false(is.null(attr(z, "scaled:scale")))
  
})


test_that("all four combinations of center and scale behave", {
  
  x <- cbind(a = c(1, 2, 3, 4, 100), b = c(10, 20, 30, 40, 50))
  
  zTT <- scaleX(x, center = TRUE,  scale = TRUE)
  zTF <- scaleX(x, center = TRUE,  scale = FALSE)
  zFT <- scaleX(x, center = FALSE, scale = TRUE)
  zFF <- scaleX(x, center = FALSE, scale = FALSE)
  
  # attributes are set only for the operations actually performed
  expect_false(is.null(attr(zTT, "scaled:center")))
  expect_false(is.null(attr(zTT, "scaled:scale")))
  
  expect_false(is.null(attr(zTF, "scaled:center")))
  expect_null(attr(zTF, "scaled:scale"))
  
  expect_null(attr(zFT, "scaled:center"))
  expect_false(is.null(attr(zFT, "scaled:scale")))
  
  expect_null(attr(zFF, "scaled:center"))
  expect_null(attr(zFF, "scaled:scale"))
  
  # unname() strips names but leaves the scaled:* attributes in place,
  # so the comparison values are taken bare.
  .bare <- function(m) matrix(as.vector(m), nrow = nrow(m))

  expect_equal(.bare(zFF), .bare(x))
  expect_equal(.bare(zTF), .bare(sweep(x, 2, apply(x, 2, median), "-")))
  expect_equal(.bare(zFT), .bare(sweep(x, 2, apply(x, 2, mad), "/")))
  
})


test_that("numeric center and scale vectors are used directly", {
  
  x <- matrix(1:6, ncol = 2)
  z <- scaleX(x, center = c(0, 0), scale = c(1, 2))
  
  expect_equal(attr(z, "scaled:center"), c(0, 0))
  expect_equal(attr(z, "scaled:scale"), c(1, 2))
  expect_equal(as.vector(z), c(1, 2, 3, 2, 2.5, 3))
  
  expect_error(scaleX(x, center = c(1, 2, 3)), "length 2")
  expect_error(scaleX(x, scale = c(1, NA)), "missing values")
  expect_error(scaleX(x, center = "a"), "logical or numeric")
  
})


test_that("zero MAD warns and does not stop", {
  
  x <- cbind(constant = rep(1, 5), varying = c(1, 2, 3, 4, 5))
  
  expect_warning(z <- scaleX(x), "Scaling factor is zero")
  expect_true(all(is.nan(z[, "constant"])))
  expect_false(any(is.nan(z[, "varying"])))
  
  # a supplied zero divisor is reported as a scaling factor, not as a MAD
  w <- tryCatch(scaleX(matrix(1:4, ncol = 2), scale = c(0, 1)),
                warning = conditionMessage)
  expect_match(w, "Scaling factor")
  expect_false(grepl("MAD", w))
  
})


test_that("unnamed columns are reported by index", {
  
  x <- cbind(rep(1, 4), c(1, 2, 3, 4))
  expect_warning(scaleX(x), "column\\(s\\) 1")
  
})


test_that("fully missing columns are handled", {
  
  x <- cbind(allNA = rep(NA_real_, 5), ok = c(1, 2, 3, 4, 5))
  
  # na.rm = TRUE: median/mad of an all-NA column are NA, so the factors
  # are non-finite and must be flagged rather than silently propagated
  expect_warning(z <- scaleX(x, na.rm = TRUE))
  expect_true(all(is.na(z[, "allNA"])))
  
})


test_that("na.rm controls the statistics but never removes rows", {
  
  x <- cbind(a = c(1, 2, NA, 4, 5), b = c(10, 20, 30, 40, 50))
  
  zT <- scaleX(x, na.rm = TRUE)
  
  # with na.rm = FALSE the median/mad of column 'a' are NA, so the
  # scaling factor is non-finite and the degeneracy warning fires
  expect_warning(zF <- scaleX(x, na.rm = FALSE), "Scaling factor")
  
  # the result keeps every input row either way
  expect_equal(nrow(zT), nrow(x))
  expect_equal(nrow(zF), nrow(x))
  
  # NA entries stay NA
  expect_true(is.na(zT[3, "a"]))
  
  # with na.rm = FALSE the column statistics themselves become NA
  expect_true(is.na(attr(zF, "scaled:center")[["a"]]))
  expect_false(is.na(attr(zT, "scaled:center")[["a"]]))
  
  expect_error(scaleX(x, na.rm = NA), "non-missing logical")
  
})


test_that("non-numeric input is rejected early", {
  
  df <- data.frame(a = 1:3, b = letters[1:3])
  expect_error(scaleX(df), "must be numeric")
  
})
