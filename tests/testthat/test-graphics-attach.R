
test_that("aurora functions are available when installed", {
  
  skip_if_not_installed("aurora")
  
  library(DescToolsX)
  # expect_true(exists("PlotFdist", mode = "function"))
  
})

