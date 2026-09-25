
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


# Additional branch coverage and reference checks
test_that("nctCI at t=0 has exact normal-quantile limits for every df", {
  # The denominator of a noncentral t is positive, so P(T <= 0) = Phi(-ncp).
  for (df in c(2, 20, 100)) {
    expect_equal(unname(DescToolsX:::.nctCI(0, df)), qnorm(c(0.025, 0.975)), tolerance = 1e-7)
    expect_equal(unname(DescToolsX:::.nctCI(0, df, sides = "left")),
                 c(qnorm(0.05), Inf), tolerance = 1e-7)
    expect_equal(unname(DescToolsX:::.nctCI(0, df, sides = "right")),
                 c(-Inf, qnorm(0.95)), tolerance = 1e-7)
  }
  expect_error(DescToolsX:::.nctCI(0, 20, sides = "invalid"), "'sides'")
})

test_that("nctCI endpoints solve the specified tails and respect sign symmetry", {
  a <- DescToolsX:::.nctCI(2.5, 20)
  expect_equal(pt(2.5, 20, ncp = a[["lci"]]), 0.975, tolerance = 1e-7)
  expect_equal(pt(2.5, 20, ncp = a[["uci"]]), 0.025, tolerance = 1e-7)
  expect_equal(unname(DescToolsX:::.nctCI(-2.5, 20)), -rev(unname(a)), tolerance = 1e-7)
})

test_that("nctCI accuracy warning can be explicitly disabled", {
  expect_warning(a <- DescToolsX:::.nctCI(50, 100), "37.62")
  expect_silent(b <- DescToolsX:::.nctCI(50, 100, warnLimit = FALSE))
  expect_equal(a, b)
})
