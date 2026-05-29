
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
