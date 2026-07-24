test_that("scaleX returns a matrix", {
  x <- rnorm(20)
  expect_true(is.matrix(scaleX(x, robust = TRUE)))
})

test_that("scaleX centered result has median near 0", {
  set.seed(1)
  x <- rnorm(200, mean = 5, sd = 2)
  res <- scaleX(x, robust = TRUE)
  expect_lt(abs(median(res)), 0.1)
})

test_that("scaleX scaled result has mad near 1", {
  set.seed(2)
  x <- rnorm(200, mean = 5, sd = 2)
  res <- scaleX(x, robust = TRUE)
  expect_equal(mad(res), 1, tolerance = 0.05)
})

test_that("scaleX center = FALSE skips centering", {
  x <- matrix(c(1,2,3,4,5,6), ncol=2)
  res <- scaleX(x, center = FALSE, scale = FALSE, robust = TRUE)
  expect_equal(res[,1], x[,1])
})

test_that("scaleX scale = FALSE skips scaling", {
  x <- matrix(c(1,2,3,10,20,30), ncol=2)
  res <- scaleX(x, scale = FALSE)
  # column should have same spread but shifted to mean = 0
  expect_equal(mean(res[,1]), 0, tolerance = 1e-10)
  expect_equal(sd(res[,1]), sd(x[,1]), tolerance = 1e-10)
})

test_that("scaleX result has 'scaled:center' attribute", {
  x <- rnorm(30, mean=5)
  res <- scaleX(x, robust = TRUE)
  expect_false(is.null(attr(res, "scaled:center")))
})

test_that("scaleX result has 'scaled:scale' attribute", {
  x <- rnorm(30, sd=3)
  res <- scaleX(x, robust = TRUE)
  expect_false(is.null(attr(res, "scaled:scale")))
})

test_that("scaleX works column-wise for matrices", {
  x <- matrix(c(rnorm(20, mean=0), rnorm(20, mean=10)), ncol=2)
  res <- scaleX(x, robust = TRUE)
  expect_lt(abs(median(res[,1])), 0.2)
  expect_lt(abs(median(res[,2])), 0.2)
})


test_that("scaleX centers and scales by median and MAD", {
  
  x <- cbind(a = c(1, 2, 3, 4, 100), b = c(10, 20, 30, 40, 50))
  z <- scaleX(x, robust = TRUE)
  
  expect_equal(attr(z, "scaled:center"), apply(x, 2, median))
  expect_equal(attr(z, "scaled:scale"), apply(x, 2, mad))
  expect_equal(as.vector(z[, "b"]), (x[, "b"] - 30) / mad(x[, "b"]))
  
})


test_that("both scaled: attributes survive when center and scale are used", {
  
  # regression test: chaining two scale() calls dropped "scaled:center"
  z <- scaleX(cbind(a = 1:5, b = 6:10), robust = TRUE)
  
  expect_false(is.null(attr(z, "scaled:center")))
  expect_false(is.null(attr(z, "scaled:scale")))
  
})


test_that("all four combinations of center and scale behave", {
  
  x <- cbind(a = c(1, 2, 3, 4, 100), b = c(10, 20, 30, 40, 50))
  
  zTT <- scaleX(x, center = TRUE,  scale = TRUE,  robust = TRUE)
  zTF <- scaleX(x, center = TRUE,  scale = FALSE, robust = TRUE)
  zFT <- scaleX(x, center = FALSE, scale = TRUE,  robust = TRUE)
  zFF <- scaleX(x, center = FALSE, scale = FALSE, robust = TRUE)
  
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
  
  expect_warning(z <- scaleX(x, robust = TRUE), "Scaling factor is zero")
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
  expect_warning(scaleX(x, robust = TRUE), "column\\(s\\) 1")
  
})


test_that("fully missing columns are handled", {
  
  x <- cbind(allNA = rep(NA_real_, 5), ok = c(1, 2, 3, 4, 5))
  
  # na.rm = TRUE: median/mad of an all-NA column are NA, so the factors
  # are non-finite and must be flagged rather than silently propagated
  expect_warning(z <- scaleX(x, robust = TRUE, na.rm = TRUE))
  expect_true(all(is.na(z[, "allNA"])))
  
})


test_that("na.rm controls the statistics but never removes rows", {
  
  x <- cbind(a = c(1, 2, NA, 4, 5), b = c(10, 20, 30, 40, 50))
  
  zT <- scaleX(x, robust = TRUE, na.rm = TRUE)
  
  # with na.rm = FALSE the median/mad of column 'a' are NA, so the
  # scaling factor is non-finite and the degeneracy warning fires
  expect_warning(zF <- scaleX(x, robust = TRUE, na.rm = FALSE), "Scaling factor")
  
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


test_that("scaleX defaults to the conventional version", {

  x <- cbind(a = c(1, 2, 3, 4, 100), b = c(10, 20, 30, 40, 50))

  # robust = FALSE is the default, so mean and sd are used
  expect_equal(attr(scaleX(x), "scaled:center"), colMeans(x))
  expect_equal(attr(scaleX(x), "scaled:scale"), apply(x, 2, sd))

})


test_that("scaleX with robust = FALSE reproduces base::scale", {

  x <- cbind(a = c(1, 2, 3, 4, 100), b = c(10, 20, 30, 40, 50))

  for(cen in c(TRUE, FALSE)) {

    for(sca in c(TRUE, FALSE)) {

      mine <- scaleX(x, center = cen, scale = sca)
      base <- scale(x, center = cen, scale = sca)

      expect_equal(as.vector(mine), as.vector(base))
      expect_equal(attr(mine, "scaled:center"), attr(base, "scaled:center"))
      expect_equal(attr(mine, "scaled:scale"), attr(base, "scaled:scale"))

    }

  }

})


test_that("scaleX robust version resists a single extreme value", {

  x <- cbind(a = c(1, 2, 3, 4, 100))

  conventional <- attr(scaleX(x), "scaled:scale")
  robust <- attr(scaleX(x, robust = TRUE), "scaled:scale")

  # the outlier inflates the standard deviation but barely moves the MAD
  expect_gt(conventional, 10 * robust)

  expect_equal(unname(robust), mad(x[, 1]))
  expect_equal(unname(attr(scaleX(x, robust = TRUE), "scaled:center")),
               median(x[, 1]))

})


test_that("scaleX robust scale does not depend on center", {

  x <- cbind(a = c(1, 2, 3, 4, 100), b = c(10, 20, 30, 40, 50))

  # the MAD is invariant to location, so centering first changes nothing
  expect_equal(
    attr(scaleX(x, center = TRUE,  scale = TRUE, robust = TRUE), "scaled:scale"),
    attr(scaleX(x, center = FALSE, scale = TRUE, robust = TRUE), "scaled:scale")
  )

  # the root mean square is not, so the conventional version does differ
  expect_false(isTRUE(all.equal(
    attr(scaleX(x, center = TRUE,  scale = TRUE), "scaled:scale"),
    attr(scaleX(x, center = FALSE, scale = TRUE), "scaled:scale")
  )))

})


test_that("scaleX validates robust", {

  x <- matrix(1:6, ncol = 2)

  expect_error(scaleX(x, robust = NA), "non-missing logical")
  expect_error(scaleX(x, robust = "yes"), "non-missing logical")
  expect_error(scaleX(x, robust = c(TRUE, FALSE)), "non-missing logical")

})
