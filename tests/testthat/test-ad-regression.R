
test_that("Regression vs goftest", {
  
  skip_if_not_installed("goftest")
  
  # Anderson–Darling test statistic + p-value
  set.seed(1)
  x <- runif(100)
  
  ref  <- goftest::ad.test(x)
  mine <- DescToolsX::andersonDarlingTest(x)
  
  expect_equal(
    unname(mine$statistic),
    unname(ref$statistic),
    tolerance = 1e-12
  )
  
  expect_equal(
    mine$p.value,
    ref$p.value,
    tolerance = 1e-12
  )
  
  
  # multiple sample sizes
  for (n in c(10, 20, 50, 100, 500)) {
    set.seed(n)
    x <- runif(n)
    
    ref  <- goftest::ad.test(x)
    mine <- DescToolsX::andersonDarlingTest(x)
    
    expect_equal(unname(mine$statistic),
                 unname(ref$statistic),
                 tolerance = 1e-12)
    
    expect_equal(mine$p.value,
                 ref$p.value,
                 tolerance = 1e-12)
  }

})