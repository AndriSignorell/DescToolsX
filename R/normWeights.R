

.normWeights <- function(x, weights, na.rm = FALSE, normwt = FALSE) {
  
  # Idea Henrik Bengtsson
  # Remove observations with zero weights.
  # This:
  #   1) handles the case where all weights are zero,
  #   2) avoids unnecessary work in downstream computations,
  #   3) may improve sorting performance.
  
  if (na.rm) {
    
    keep <- !is.na(x) & !is.na(weights) & (weights > 0)
    
    x <- x[keep]
    weights <- weights[keep]
  }
  
  if (any(is.na(x)) || (!is.null(weights) && any(is.na(weights))))
    return(NA_real_)
  
  n <- length(x)
  
  if (length(weights) != n)
    stop("length of 'weights' must equal the number of rows in 'x'")
  
  # x and weights have length 0
  if (length(x) == 0)
    return(list(x = x, weights = x, wsum = NaN))
  
  if (any(weights < 0) || (s <- sum(weights)) == 0)
    stop("weights must be non-negative and not all zero")
  
  if (normwt)
    weights <- weights * n / s
  
  list(
    x = x,
    weights = as.double(weights),
    wsum = s
  )
}

