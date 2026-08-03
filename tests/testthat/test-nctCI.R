
test_that("there is exactly one .nctCI and it returns named limits", {
  
  ns <- asNamespace("DescToolsX")
  expect_true(exists(".nctCI", envir = ns, inherits = FALSE))
  
  # both callers index the result by name
  lim <- DescToolsX:::.nctCI(2.5, df = 20, conf.level = 0.95)
  expect_named(lim, c("lci", "uci"))
  expect_lt(lim[["lci"]], lim[["uci"]])
  
  one <- DescToolsX:::.nctCI(2.5, df = 20, conf.level = 0.95, sides = "left")
  expect_identical(unname(one[["uci"]]), Inf)
  expect_true(is.finite(one[["lci"]]))
})

