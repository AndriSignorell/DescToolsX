## ============================================================
## BCa adjustment, on the probability scale
## ============================================================
##
## The two quantities BCa needs - the bias correction z0 and the
## acceleration a - only ever enter through the two probabilities at which
## the replicates are read. Computing those probabilities here rather than
## indexing the sorted replicates in C++ has three consequences worth the
## file: quantile() cannot reach past the end of the vector, "perc" and
## "bca" use the same quantile convention by construction, and both edge
## cases below can be tested without a compiler.


# Bias correction from the position of the estimate within the replicates.
#' @noRd
.bcaZ0 <- function(t, t0) {

  R <- length(t)

  # Ties count half. C is a discrete statistic - on a small table a good
  # share of the replicates land exactly on the estimate - and counting
  # only the strict "<" pushes z0 towards -Inf for no reason.
  prop <- (sum(t < t0) + 0.5 * sum(t == t0)) / R

  # qnorm(0) and qnorm(1) are infinite. Staying half a replicate away from
  # either end is the usual convention and keeps z0 finite when the
  # estimate sits at the edge of the bootstrap distribution, which for an
  # association measure near zero is the normal case, not a pathology.
  stats::qnorm(min(max(prop, 0.5 / R), 1 - 0.5 / R))
}


# Map the percentile probabilities onto the BCa-adjusted ones.
#' @noRd
.bcaProbs <- function(probs, z0, a) {

  # A one-sided interval passes 0 or 1 for the open side; that side is
  # closed at the range boundary by the caller and must not be adjusted
  # (qnorm(1) is Inf and would poison the arithmetic).
  ok <- probs > 0 & probs < 1

  if (!any(ok))
    return(probs)

  z   <- stats::qnorm(probs[ok])
  den <- 1 - a * (z0 + z)
  adj <- stats::pnorm(z0 + (z0 + z) / den)

  # BCa is defined only where all of this is finite and the denominator
  # stays positive; a non-positive denominator turns the map non-monotone,
  # so the "interval" it produces is not one. Falling back to the
  # percentile bounds is the honest answer - and it is said out loud,
  # because the two intervals are not interchangeable.
  if (!is.finite(z0) || !is.finite(a) ||
      any(!is.finite(den)) || any(den <= 0) || any(!is.finite(adj))) {
    warning("the BCa adjustment is not defined for this sample; ",
            "reporting percentile bounds instead", call. = FALSE)
    return(probs)
  }

  probs[ok] <- adj
  probs
}
