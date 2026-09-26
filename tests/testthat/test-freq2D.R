
test_that("freq2D returns a matrix-like table", {
  res <- freq2D(quakes$long, quakes$lat)
  expect_true(is.matrix(res) || is.table(res))
})

test_that("freq2D total counts equal number of observations", {
  res <- freq2D(quakes$long, quakes$lat)
  expect_equal(sum(res), nrow(quakes))
})

test_that("freq2D all counts are non-negative integers", {
  res <- freq2D(quakes$long, quakes$lat)
  expect_true(all(res >= 0))
  expect_true(all(res == floor(res)))
})

test_that("freq2D formula interface gives same total as default", {
  res_def  <- freq2D(quakes$long, quakes$lat)
  res_form <- freq2D(lat ~ long, data = quakes)
  expect_equal(sum(res_def), sum(res_form))
})

test_that("freq2D formula interface forwards n, pad, and dnn", {
  res_def <- freq2D(
    quakes$long, quakes$lat,
    n = c(10, 15), pad = 1, dnn = c("lon", "lat")
  )
  res_form <- freq2D(
    lat ~ long, data = quakes,
    n = c(10, 15), pad = 1, dnn = c("lon", "lat")
  )

  expect_equal(dim(res_form), dim(res_def))
  expect_equal(as.vector(res_form), as.vector(res_def))
  expect_equal(names(dimnames(res_form)), names(dimnames(res_def)))
})

test_that("freq2D n argument changes number of bins", {
  r10 <- freq2D(quakes$long, quakes$lat, n = 10)
  r5  <- freq2D(quakes$long, quakes$lat, n = 5)
  expect_gte(prod(dim(r10)), prod(dim(r5)))
})

test_that("freq2D pad argument adds zero-filled margins", {
  r0 <- freq2D(quakes$long, quakes$lat, pad = 0)
  r1 <- freq2D(quakes$long, quakes$lat, pad = 1)
  expect_equal(dim(r1)[1], dim(r0)[1] + 2)
  expect_equal(dim(r1)[2], dim(r0)[2] + 2)
})

test_that("freq2D dimnames are set when dnn is provided", {
  res <- freq2D(quakes$long, quakes$lat, dnn = c("lon","lat"))
  # freq2D transposes the result so y is rows and x is columns
  # → dimnames order in the output is c("lat","lon")
  expect_equal(sort(names(dimnames(res))), sort(c("lon","lat")))
})



test_that("freq2D copes with a single occupied bin row", {
  
  x <- c(1, 1, 1, 1)
  y <- c(1, 2, 3, 4)
  
  # trimming empty margins used to drop the matrix to a vector
  expect_silent(z <- freq2D(x, y, n = 5))
  expect_true(is.matrix(z))
})


# Formula interface via resolveFormulaFromCall() ------------------------------

test_that("freq2D formula interface evaluates subset in data", {
  res  <- freq2D(lat ~ long, data = quakes, subset = mag > 5)
  expect_equal(sum(res), sum(quakes$mag > 5))

  sel <- quakes$mag > 5
  expect_equal(as.vector(res),
               as.vector(freq2D(quakes$long[sel], quakes$lat[sel])))
})

test_that("freq2D formula interface finds subset variables of a calling function", {
  f <- function(m) freq2D(lat ~ long, data = quakes, subset = mag > m)
  expect_equal(sum(f(5.5)), sum(quakes$mag > 5.5))
})

test_that("freq2D formula interface sets data.name", {
  # used to be NULL: the field is 'dataName', not 'data.name'
  expect_identical(attr(freq2D(lat ~ long, data = quakes), "data.name"),
                   "lat ~ long")
})

test_that("freq2D formula interface drops incomplete pairs", {
  q <- quakes
  q$lat[1:3] <- NA
  expect_equal(sum(freq2D(lat ~ long, data = q)), nrow(q) - 3L)
})

test_that("freq2D formula interface requires two numeric variables", {
  q <- transform(quakes, g = factor(stations > 30))
  expect_error(freq2D(lat ~ g, data = q))
})
