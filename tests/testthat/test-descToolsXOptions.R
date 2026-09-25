test_that("setDescToolsXOption() stores prefixed options, returns old values", {
  withr::local_options(list(DescToolsX.testopt = "old"))
  old <- setDescToolsXOption(testopt = "new")
  expect_identical(getOption("DescToolsX.testopt"), "new")
  expect_identical(old, list(DescToolsX.testopt = "old"))
})

test_that("setDescToolsXOption() returns invisibly and sets several at once", {
  withr::local_options(list(DescToolsX.testopt = NULL, DescToolsX.testopt2 = NULL))
  expect_invisible(setDescToolsXOption(testopt = 1, testopt2 = 2))
  expect_identical(getOption("DescToolsX.testopt2"), 2)
})

test_that("setDescToolsXOption() rejects empty and unnamed input", {
  expect_error(setDescToolsXOption(), "name = value")
  expect_error(setDescToolsXOption(1), "name = value")
  expect_error(setDescToolsXOption(a = 1, 2), "name = value")
})

test_that(".getOption() reads the prefixed option with a default", {
  withr::local_options(list(DescToolsX.testopt = NULL))
  expect_identical(.getOption("testopt", "def"), "def")
  setDescToolsXOption(testopt = "set")
  expect_identical(.getOption("testopt", "def"), "set")
})
