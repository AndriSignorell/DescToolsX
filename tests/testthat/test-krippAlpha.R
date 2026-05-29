.kdat_nom <- data.frame(
  r1 = c(1, 2, 1, 3, 2),
  r2 = c(1, 2, 2, 3, 2),
  r3 = c(1, 2, 1, 3, 1)
)

test_that("krippAlpha returns a single numeric for nominal data (no CI)", {
  res <- krippAlpha(.kdat_nom, method = "nominal")
  expect_type(res, "double")
  expect_length(res, 1)
})

test_that("krippAlpha perfect agreement gives alpha = 1", {
  dat <- data.frame(r1=1:5, r2=1:5, r3=1:5)
  expect_equal(krippAlpha(dat, method="nominal"), 1, tolerance=1e-6)
})

test_that("krippAlpha nominal result is in [-1, 1]", {
  res <- krippAlpha(.kdat_nom, method = "nominal")
  expect_gte(res, -1); expect_lte(res, 1)
})

test_that("krippAlpha ordinal method returns a numeric in [-1, 1]", {
  res <- krippAlpha(.kdat_nom, method = "ordinal")
  expect_gte(res, -1); expect_lte(res, 1)
})

test_that("krippAlpha interval method returns a numeric", {
  dat <- data.frame(r1=c(1,4,5,7,2), r2=c(2,5,6,7,1), r3=c(1,4,6,6,2))
  res <- krippAlpha(dat, method="interval", levels=1:7)
  expect_true(is.numeric(res))
})

test_that("krippAlpha ratio method returns a numeric", {
  dat <- data.frame(r1=c(1,4,5,7,2), r2=c(2,5,6,7,1), r3=c(1,4,6,6,2))
  res <- krippAlpha(dat, method="ratio", levels=1:7)
  expect_true(is.numeric(res))
})

test_that("krippAlpha handles NAs in data", {
  dat_na <- .kdat_nom
  dat_na[1,1] <- NA
  expect_true(is.numeric(krippAlpha(dat_na, method = "nominal")))
})

test_that("krippAlpha out = 'ext' returns a list with alpha and O", {
  res <- krippAlpha(.kdat_nom, method="nominal", out="ext")
  expect_type(res, "list")
  expect_true(all(c("alpha","Do","De","O") %in% names(res)))
})

test_that("krippAlpha conf.level returns named vector est/lci/uci", {
  res <- krippAlpha(.kdat_nom, method="nominal", conf.level=0.95, R=200)
  expect_length(res, 3)
  expect_named(res, c("est","lci","uci"))
})
