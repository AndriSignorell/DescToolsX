

# Bring x and weights into a common, validated shape.
#
# Returns a list with the (possibly filtered) x, the weights as doubles,
# and their sum. Weights are NOT rescaled unless normwt = TRUE - callers
# that need an effective sample size have to ask for it. This matters:
# quantileX(type = 7) reads weights as replication counts, so a caller
# that hands on rescaled weights changes that function's answer.
#' @noRd
.normWeights <- function(x, weights, na.rm = FALSE, normwt = FALSE) {

  if (is.null(weights))
    stop("'weights' must not be NULL")

  # Length first, and on the ORIGINAL vectors. The check used to sit
  # after the filtering below, where `keep` had already recycled the
  # shorter of the two against the longer - so a genuine length mismatch
  # was either silently absorbed or reported against post-filter lengths
  # that no caller ever passed.
  if (length(weights) != length(x))
    stop("length of 'weights' must equal the length of 'x'")

  # Idea Henrik Bengtsson
  # Remove observations with zero weights.
  # This:
  #   1) handles the case where all weights are zero,
  #   2) avoids unnecessary work in downstream computations,
  #   3) may improve sorting performance.
  #
  # The removal used to sit inside the na.rm branch, so it only happened
  # when missing values were being dropped as well - although the comment
  # describes it as unconditional and nothing about a zero weight depends
  # on na.rm. Separated: NAs go with na.rm, zero weights always.
  if (na.rm) {
    keep <- !is.na(x) & !is.na(weights)
    x <- x[keep]
    weights <- weights[keep]
  }

  if (anyNA(x) || anyNA(weights)) {
    # Shape, not a bare scalar. The former `return(NA_real_)` handed back
    # an atomic value to callers that immediately write z$x, z$weights
    # and z$wsum - so meanX(c(1, 2, NA), weights = c(1, 1, 1)) died with
    # "$ operator is invalid for atomic vectors" instead of returning NA.
    return(list(x = NA_real_, weights = NA_real_, wsum = NA_real_))
  }

  if (length(x) == 0L)
    return(list(x = x, weights = as.double(x), wsum = NaN))

  if (any(weights < 0))
    stop("'weights' must be non-negative")

  s <- sum(weights)

  if (s == 0)
    stop("'weights' must not be all zero")

  keep <- weights > 0
  if (!all(keep)) {
    x <- x[keep]
    weights <- weights[keep]
  }

  if (normwt)
    weights <- weights * length(x) / s

  list(
    x = x,
    weights = as.double(weights),
    wsum = s
  )
}
