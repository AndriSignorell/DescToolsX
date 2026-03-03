
library(testthat)

tol <- 1e-3

# https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf, page 9
scenarios <- list(
  s1 = list(x1=56, n1=70, x2=48, n2=80),
  s2 = list(x1=9,  n1=10, x2=3,  n2=10),
  s3 = list(x1=10, n1=10, x2=0,  n2=20)
)

expected <- list(
  
  wald = list(
    s1 = c(0.0575, 0.3425),
    s2 = c(0.2605, 0.9395),
    s3 = c(1.0000, 1.0000)
  ),
  
  `wald-cc` = list(
    s1 = c(0.0441, 0.3559),
    s2 = c(0.1605, 1.0000),
    s3 = c(0.9250, 1.0000)
  ),
  
  haldane = list(
    s1 = c(0.0535, 0.3351),
    s2 = c(0.1777, 0.8289),
    s3 = c(0.7482, 1.0000)
  ),
  
  `jeffreys-perks` = list(
    s1 = c(0.0531, 0.3355),
    s2 = c(0.1760, 0.8306),
    s3 = c(0.7431, 1.0000)
  ),
  
  `mee-farrington-manning` = list(
    s1 = c(0.0533, 0.3377),
    s2 = c(0.1821, 0.8370),
    s3 = c(0.7225, 1.0000)
  ),
  
  `miettinen-nurminen` = list(
    s1 = c(0.0528, 0.3382),
    s2 = c(0.1700, 0.8406),
    s3 = c(0.7156, 1.0000)
  ),
  
  # exact = list(
  #   s1 = c(0.0529, 0.3403),
  #   s2 = c(0.1393, 0.8836),
  #   s3 = c(0.6915, 1.0000)
  # ),
  
  `newcombe-score` = list(
    s1 = c(0.0524, 0.3339),
    s2 = c(0.1705, 0.8090),
    s3 = c(0.6791, 1.0000)
  ),
  
  `newcombe-score-cc` = list(
    s1 = c(0.0428, 0.3422),
    s2 = c(0.1013, 0.8387),
    s3 = c(0.6014, 1.0000)
  ),
  
  `hauck-anderson` = list(
    s1 = c(0.0494, 0.3506),
    s2 = c(0.1922, 1.0000),
    s3 = c(0.9500, 1.0000)
  ),
  
  `agresti-caffo` = list(
    s1 = c(0.0525, 0.3358),
    s2 = c(0.1600, 0.8400),
    s3 = c(0.6922, 1.0000)
  )
)

test_that("diffCI matches SAS reference values", {
  
  for (m in names(expected)) {
    
    for (s in names(scenarios)) {
      
      sc <- scenarios[[s]]
      
      ci <- DescToolsX::binomDiffCI(
        sc$x1, sc$n1,
        sc$x2, sc$n2,
        method = m
      )
      
      ci <- as.numeric(ci[c("lci", "uci")])
      
      expect_equal(ci, expected[[m]][[s]], tolerance = tol)
      
    }
  }
})


# same reference: diffCI matches HIV clinical trial reference values

x1 <- 84; n1 <- 101
x2 <- 89; n2 <- 105

expected <- list(
  wald                     = c(-0.1162,  0.0843),
  `wald-cc`                = c(-0.1259,  0.0940),
  haldane                  = c(-0.1152,  0.0834),
  `jeffreys-perks`         = c(-0.1160,  0.0843),
  `mee-farrington-manning` = c(-0.1188,  0.0857),
  `miettinen-nurminen`     = c(-0.1191,  0.0860),
  `newcombe-score`         = c(-0.1177,  0.0851),
  `newcombe-score-cc`      = c(-0.1245,  0.0918),
  `hauck-anderson`         = c(-0.1216,  0.0898),
  `agresti-caffo`          = c(-0.1168,  0.0850)
)

test_that("diffCI matches HIV clinical trial reference values", {
  
  for (m in names(expected)) {
    
    ci <- binomDiffCI(
      x1, n1,
      x2, n2,
      method = m
    )
    
    ci <- as.numeric(ci[c("lci", "uci")])
    
    expect_equal(ci, expected[[m]], tolerance = tol)
    
  }
})


