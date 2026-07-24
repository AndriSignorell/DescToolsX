
#' Robust Scaling With Median and MAD
#'
#' \code{scaleX} performs robust standardization, using
#' \code{\link[stats]{median}} and \code{\link[stats]{mad}} instead of
#' \code{\link[base]{mean}} and \code{\link[stats]{sd}}.
#'
#' The function mirrors the interface of \code{\link[base]{scale}}: both
#' \code{center} and \code{scale} accept either a logical flag or a numeric
#' vector of values to use directly, in which case that vector must have one
#' entry per column of \code{x}.
#'
#' Because the median absolute deviation is invariant to location shifts, it
#' has the same value whether computed before or after centering. The scale
#' factors are therefore computed from the original columns, so that the
#' returned \code{"scaled:scale"} remains interpretable independently of
#' \code{center}.
#'
#' A zero or non-finite scaling factor can produce undefined or non-finite
#' results. \code{scaleX} emits a warning naming the affected columns rather
#' than failing, since the result may still be usable when those columns are
#' subsequently dropped.
#'
#' @param x a numeric matrix-like object
#' @param center logical scalar or numeric vector. If \code{TRUE}, the column
#' medians are subtracted; if \code{FALSE}, no centering is performed.
#' Alternatively, a numeric vector of length \code{ncol(x)} supplies the
#' values to subtract directly.
#' @param scale logical scalar or numeric vector. If \code{TRUE}, the
#' (centered) columns are divided by their MAD; if \code{FALSE}, no scaling
#' is performed. Alternatively, a numeric vector of length \code{ncol(x)}
#' supplies the divisors directly.
#' @param na.rm logical; if \code{TRUE} (default), missing values are omitted
#' when the column medians and MADs are computed. Ignored for whichever of
#' \code{center} and \code{scale} is given as a numeric vector. Missing
#' entries of \code{x} itself always remain missing in the result.
#'
#' @return the centered and scaled matrix. The numeric centerings and scalings used (if
#' any) are returned as attributes \code{"scaled:center"} and
#' \code{"scaled:scale"}.
#'
#' @seealso \code{\link[base]{scale}}, \code{\link[base]{sweep}},
#' \code{\link[stats]{mad}}
#'
#' @family transform
#' @concept robust-statistic
#' @concept standardization
#' @concept outlier-resistance
#'
#' @examples
#' x <- bedrock::Pizza$temperature
#'
#' # robust standardization is far less affected by the extreme values
#' plot(scaleX(x), col = "black", pch = 16, cex = 0.4,
#'      ylab = "standardized temperature")
#' points(scale(x), col = "red", pch = 16, cex = 0.4)
#' legend("topright", legend = c("scaleX (median/MAD)", "scale (mean/SD)"),
#'        col = c("black", "red"), pch = 16, bty = "n")
#'
#' # the centerings and scalings used are recoverable
#' z <- scaleX(cbind(a = c(1, 2, 3, 4, 100), b = c(10, 20, 30, 40, 50)))
#' attr(z, "scaled:center")
#' attr(z, "scaled:scale")
#'
#' # supplying the values directly, as base::scale allows
#' scaleX(matrix(1:6, ncol = 2), center = c(0, 0), scale = c(1, 2))
#'
#' @export
scaleX <- function(x, center = TRUE, scale = TRUE, na.rm = TRUE){

  x <- as.matrix(x)

  # as.matrix() on a data frame holding any non-numeric column silently
  # coerces every column to character; median() would then fail deep
  # inside apply() with an opaque message. Checked here instead.
  if(!is.numeric(x))
    stop("Argument 'x' must be numeric.")

  if(!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("Argument 'na.rm' must be a single non-missing logical value.")

  nCol <- ncol(x)

  # center and scale mirror base::scale, which accepts either a logical
  # flag or a ready-made numeric vector of values.
  .resolve <- function(value, name, fun) {

    if(is.logical(value)) {

      if(length(value) != 1L || is.na(value))
        stop(gettextf("Argument '%s' must be a single non-missing logical value, or a numeric vector.",
                      name), domain = NA)

      if(!value)
        return(NULL)

      return(apply(x, 2, fun, na.rm = na.rm))

    }

    if(!is.numeric(value))
      stop(gettextf("Argument '%s' must be logical or numeric.", name),
           domain = NA)

    if(length(value) != nCol)
      stop(gettextf("Argument '%s' must have length %d, matching ncol(x).",
                    name, nCol), domain = NA)

    if(anyNA(value))
      stop(gettextf("Argument '%s' must not contain missing values.", name),
           domain = NA)

    value

  }

  # Computed from the original columns. mad() is invariant to location
  # shifts, so this yields the same factors that centering first would,
  # while keeping the returned "scaled:scale" independent of 'center'.
  centerVals <- .resolve(center, "center", median)
  scaleVals <- .resolve(scale, "scale", mad)

  if(!is.null(scaleVals)) {

    # Phrased in terms of the scaling factor rather than the MAD: when
    # 'scale' is a user-supplied vector the offending value never came
    # from mad() at all, and naming it would misdirect the reader.
    # A negative divisor is legitimate - as in base::scale() it merely
    # flips the sign - so only zero and non-finite values qualify.
    degenerate <- !is.finite(scaleVals) | scaleVals == 0

    if(any(degenerate)) {

      labels <- if(is.null(colnames(x)))
        which(degenerate)
      else
        colnames(x)[degenerate]

      warning(gettextf(
        "Scaling factor is zero or non-finite for column(s) %s; results may contain non-finite values.",
        paste(labels, collapse = ", ")), domain = NA)

    }

  }

  # sweep() rather than two chained scale() calls: scale() returns a
  # fresh matrix carrying only the attribute it has just set, so a second
  # call would silently discard "scaled:center" from the first - leaving
  # the documented return contract unmet whenever both are requested.
  if(!is.null(centerVals))
    x <- sweep(x, 2L, centerVals, "-", check.margin = FALSE)

  if(!is.null(scaleVals))
    x <- sweep(x, 2L, scaleVals, "/", check.margin = FALSE)

  # Attributes are set only for the operations actually performed, again
  # matching base::scale.
  if(!is.null(centerVals))
    attr(x, "scaled:center") <- centerVals

  if(!is.null(scaleVals))
    attr(x, "scaled:scale") <- scaleVals

  x

}
