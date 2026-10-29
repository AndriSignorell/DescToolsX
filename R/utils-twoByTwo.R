
# == shared internals for 2x2-table effect sizes ==============================
#
# Used by relRisk() and oddsRatio(). Kept in one place so that both functions
# validate their input identically and invert their interval functions with
# the same numerically safe search.


# Validate a 2x2 table of counts and return it as a double matrix.
#
# 'y' supplied: x and y are cross-tabulated with table(x, y, ...) first.
# 'cols = TRUE' additionally requires positive column totals, for measures
# that are undefined when the outcome does not vary (the odds ratio); the
# relative risk keeps its documented results for such tables.
.as2x2 <- function(x, y = NULL, ..., cols = FALSE) {

  if (!is.null(y)) {
    x <- table(x, y, ...)
  } else if (...length() > 0L) {
    # '...' only reaches table(); without 'y' an argument there is a typo or
    # a removed argument, and silently dropping it hides the mistake
    stop("Unused arguments: '...' is passed to table() only when 'y' is supplied.")
  }

  if (!is.numeric(x))
    stop("Argument 'x' must be numeric.")

  if (anyNA(x))
    stop("Argument 'x' must not contain missing values.")

  if (length(dim(x)) != 2L)
    stop("Argument 'x' must be a 2x2 matrix or table; for vector input, supply 'y'.")

  if (!all(dim(x) == c(2L, 2L)))
    stop("Argument 'x' must be a 2x2 matrix.")

  if (any(!is.finite(x)))
    stop("Argument 'x' must contain finite counts.")

  if (any(x < 0))
    stop("Argument 'x' must contain non-negative counts.")

  if (any(x != floor(x)))
    stop("Argument 'x' must contain integer counts.")

  # Use double arithmetic consistently for matrix and table inputs.
  storage.mode(x) <- "double"

  if (any(!is.finite(rowSums(x))))
    stop("Rows of 'x' must have finite totals.")

  if (any(rowSums(x) == 0))
    stop("Rows of 'x' must contain positive totals.")

  if (cols && any(colSums(x) == 0))
    stop("Columns of 'x' must contain positive totals.")

  x

}


# Validate the cell correction shared by the Wald-type intervals.
.checkDelta <- function(delta) {
  if (length(delta) != 1L || !is.numeric(delta) || !is.finite(delta) || delta < 0)
    stop("'delta' must be a single finite non-negative number.")
  delta
}


# Search outward from 'start' on the log scale until g() reaches the requested
# sign (sign = 1: g >= 0, sign = -1: g <= 0). The distance doubles each step,
# so any representable ratio is reached in about ten evaluations. Arriving at
# the limits of normal doubles without the sign change is an error: it is no
# evidence for a bound of 0 or Inf, which the callers handle explicitly.
.seekLog <- function(g, start, direction, sign) {

  limits <- c(log(.Machine$double.xmin), log(.Machine$double.xmax) - 1)
  step <- log(2)

  repeat {
    candidate <- max(limits[1L], min(limits[2L], start + direction * step))
    if (sign * g(candidate) >= 0)
      return(candidate)
    if (candidate == limits[1L] || candidate == limits[2L])
      stop("Could not bracket the root within numerical limits.")
    step <- 2 * step
  }

}


# Root of a monotone function g of the log ratio. The tolerance applies to
# the log scale and is therefore relative on the ratio scale: an absolute
# tolerance (uniroot's default, also used inside fisher.test()) loses all
# precision for small ratios.
.rootLog <- function(g, increasing, start = 0) {

  g0 <- g(start)
  if (g0 == 0)
    return(start)

  # walk towards the sign change
  direction <- if ((g0 < 0) == increasing) 1 else -1
  end <- .seekLog(g, start, direction, sign = -sign(g0))

  uniroot(g, sort(c(start, end)), tol = 1e-10, check.conv = TRUE)$root

}
