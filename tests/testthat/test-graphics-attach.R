
test_that("DescToolsGraphics functions are available when installed", {
  
  skip_if_not_installed("DescToolsGraphics")
  
  library(DescToolsX)
  # expect_true(exists("PlotFdist", mode = "function"))
  
})

