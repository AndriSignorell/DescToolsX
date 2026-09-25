

test_that("ordAssocResult strips source names and labels interval results", {
  shape <- DescToolsX:::.ordAssocResult
  expect_identical(shape(list(c(tau = 0.4)), NA_real_), 0.4)
  expect_identical(shape(list(c(tau = 0.4, lower = 0.2, upper = 0.6)), 0.95),
                   c(est = 0.4, lci = 0.2, uci = 0.6))
  expect_identical(shape(list(NA_real_), NA), NA_real_)
  expect_identical(shape(list(rep(NA_real_, 3)), 0.95),
                   c(est = NA_real_, lci = NA_real_, uci = NA_real_))
})

test_that("ordAssocResult rejects malformed estimate and interval lengths", {
  shape <- DescToolsX:::.ordAssocResult
  for (n in c(0, 2, 3))
    expect_error(shape(list(numeric(n)), NA), "single estimate was expected")
  for (n in c(0, 1, 2, 4))
    expect_error(shape(list(numeric(n)), 0.95), "estimate and interval were expected")
})
