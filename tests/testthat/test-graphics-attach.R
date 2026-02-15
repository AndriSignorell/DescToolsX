
test_that("DescToolsViz functions are available when installed", {
  
  skip_if_not_installed("DescToolsViz")
  
  library(DescToolsX)
  # expect_true(exists("PlotFdist", mode = "function"))
  
})

