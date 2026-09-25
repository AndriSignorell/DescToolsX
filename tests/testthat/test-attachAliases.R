test_that("attachAliases() assigns or/rr as the original functions", {
  e <- new.env()
  expect_message(nm <- attachAliases(envir = e), "aliases attached")
  expect_setequal(nm, c("or", "rr"))
  expect_identical(e$or, oddsRatio)
  expect_identical(e$rr, relRisk)
})

test_that("attachAliases() does not clobber existing objects", {
  e <- new.env()
  e$or <- "mine"
  suppressMessages(
    expect_warning(nm <- attachAliases(envir = e), "already in use: or"))
  expect_identical(nm, "rr")
  expect_identical(e$or, "mine")
  expect_identical(e$rr, relRisk)
})

test_that("attachAliases() attaches nothing when all names are taken", {
  e <- new.env()
  e$or <- 1
  e$rr <- 2
  expect_warning(nm <- attachAliases(envir = e), "or, rr")
  expect_length(nm, 0L)
  expect_identical(e$or, 1)
})

test_that("overwrite = TRUE replaces existing objects", {
  e <- new.env()
  e$or <- "mine"
  expect_no_warning(suppressMessages(attachAliases(envir = e, overwrite = TRUE)))
  expect_identical(e$or, oddsRatio)
})

test_that("detachAliases() removes only what attachAliases() put there", {
  e <- new.env()
  suppressMessages(attachAliases(envir = e))
  e$rr <- "mine"
  expect_message(removed <- detachAliases(envir = e), "removed \\(or\\)")
  expect_identical(removed, "or")
  expect_false(exists("or", envir = e, inherits = FALSE))
  expect_identical(e$rr, "mine")
})

test_that("detachAliases() on an environment without aliases is silent", {
  e <- new.env()
  expect_silent(removed <- detachAliases(envir = e))
  expect_identical(removed, character(0))
})
