
library(testthat)


bdci <- binomDiffCI

# helper: exact coverage calculation
.coverage_bdci <- function(n, p1, p2,
                           method = "beal",
                           conf.level = 0.95) {
  
  delta_true <- p1 - p2
  cov <- 0
  
  for (x1 in 0:n) {
    for (x2 in 0:n) {
      
      pr <- dbinom(x1, n, p1) * dbinom(x2, n, p2)
      
      ci <- bdci(x1, n, x2, n,
                 conf.level = conf.level,
                 method = method)[2:3]
      
      if (delta_true >= ci[1] && delta_true <= ci[2])
        cov <- cov + pr
    }
  }
  
  cov
}


test_that("Beal interval reproduces coverage from Beal (1987) Table 1", {
  
  # Beal Table 1 (page 6), n=5, p1=p2=0.3, nominal 0.95
  # Reported coverage for usual interval: 0.922
  # (Row 1, Table 1) :contentReference[oaicite:1]{index=1}
  
  cov_beal <- .coverage_bdci(n = 5,
                             p1 = 0.3,
                             p2 = 0.3,
                             method = "beal",
                             conf.level = 0.95)
  
  expect_equal(cov_beal,
               0.922,
               tolerance = 0.002)
})


test_that("Beal interval behaves reasonably in extreme case", {
  
  # Beal Table 2 (page 7), n=5, p1=0.9, p2=0.05
  # Used as stress test :contentReference[oaicite:2]{index=2}
  
  cov_beal <- .coverage_bdci(n = 5,
                             p1 = 0.9,
                             p2 = 0.05,
                             method = "beal",
                             conf.level = 0.95)
  
  # should not collapse like Wald
  expect_gt(cov_beal, 0.80)
  expect_lt(cov_beal, 1.00)
})
