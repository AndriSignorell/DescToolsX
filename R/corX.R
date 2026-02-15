

corX <- function (d, w = rep(1, nrow(d))/nrow(d)) {
  
  # table pearson correlation, taken for boot::corr
  s <- sum(w)
  m1 <- sum(d[, 1L] * w)/s
  m2 <- sum(d[, 2L] * w)/s
  (sum(d[, 1L] * d[, 2L] * w)/s - m1 * m2) / 
    sqrt((sum(d[, 1L]^2 * w)/s - m1^2) * 
           (sum(d[, 2L]^2 * w)/s - m2^2))
}


