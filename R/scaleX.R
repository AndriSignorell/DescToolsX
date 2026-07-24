
#' (Robust) Scaling and Centering
#'
#' Centers and scales the columns of a numeric matrix. The conventional
#' version uses mean and standard deviation, the robust one median and MAD
#' (see Details).
#'
#' The R base function \code{\link[base]{scale}} centers each column by its
#' mean and divides by the root mean square of the centered column, which
#' for centered data is the standard deviation. Both are sensitive to
#' outliers: a single extreme value moves the mean and inflates the standard
#' deviation, so the remaining observations are compressed towards zero.
#'
#' If \code{robust} is set to \code{TRUE} the column median takes the place
#' of the mean and the median absolute deviation
#' (\code{\link[stats]{mad}}) that of the standard deviation. Both have a
#' breakdown point of 50 percent, so the standardization reflects the bulk
#' of the data rather than its extremes, and genuine outliers keep large
#' scores instead of being pulled in.
#'
#' Whichever is chosen, \code{center} and \code{scale} accept either a
#' logical flag or a numeric vector of values to use directly, in which case
#' that vector must have one entry per column of \code{x}, as in
#' \code{\link[base]{scale}}.
#'
#' The two versions differ in one further respect. The MAD is invariant to
#' location shifts, so for \code{robust = TRUE} the returned
#' \code{"scaled:scale"} is the same whether or not the columns were
#' centered first, and does not depend on \code{center}. The root mean
#' square is not invariant; for \code{robust = FALSE} it is computed after
#' centering, matching \code{\link[base]{scale}}, which is what makes it
#' equal the standard deviation when \code{center} is \code{TRUE} and not
#' otherwise.
#'
#' A zero or non-finite scaling factor can produce undefined or non-finite
#' results. \code{scaleX} emits a warning naming the affected columns rather
#' than failing, since the result may still be usable when those columns are
#' subsequently dropped.
#'
#' @param x a numeric matrix-like object
#' @param center logical scalar or numeric vector. If \code{TRUE}, the column
#' means (or medians, for \code{robust = TRUE}) are subtracted; if
#' \code{FALSE}, no centering is performed. Alternatively, a numeric vector
#' of length \code{ncol(x)} supplies the values to subtract directly.
#' @param scale logical scalar or numeric vector. If \code{TRUE}, the
#' columns are divided by their standard deviation (or MAD, for
#' \code{robust = TRUE}); if \code{FALSE}, no scaling is performed.
#' Alternatively, a numeric vector of length \code{ncol(x)} supplies the
#' divisors directly.
#' @param robust logical; whether to standardize by median and MAD rather
#' than by mean and standard deviation
#' @param na.rm logical; if \code{TRUE} (default), missing values are omitted
#' when the column centers and scales are computed. Ignored for whichever of
#' \code{center} and \code{scale} is given as a numeric vector. Missing
#' entries of \code{x} itself always remain missing in the result.
#'
#' @return the centered and scaled matrix. The numeric centerings and
#' scalings used (if any) are returned as attributes
#' \code{"scaled:center"} and \code{"scaled:scale"}.
#'
#' @seealso \code{\link[base]{scale}}, \code{\link[base]{sweep}},
#' \code{\link[stats]{mad}}, \code{\link{rangeX}}
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
#' plot(scaleX(x, robust = TRUE), col = "black", pch = 16, cex = 0.4,
#'      ylab = "standardized temperature")
#' points(scaleX(x), col = "red", pch = 16, cex = 0.4)
#' legend("topright", legend = c("robust (median/MAD)", "conventional"),
#'        col = c("black", "red"), pch = 16, bty = "n")
#'
#' # the centerings and scalings used are recoverable
#' z <- scaleX(cbind(a = c(1, 2, 3, 4, 100), b = c(10, 20, 30, 40, 50)))
#' attr(z, "scaled:center")
#' attr(z, "scaled:scale")
#'
#' # compared to the robust version, which the extreme value barely moves
#' attr(scaleX(cbind(a = c(1, 2, 3, 4, 100)), robust = TRUE), "scaled:scale")
#'
#' # supplying the values directly, as base::scale allows
#' scaleX(matrix(1:6, ncol = 2), center = c(0, 0), scale = c(1, 2))
#'
#' @export
scaleX <- function(x, center = TRUE, scale = TRUE, robust = FALSE,
                   na.rm = TRUE){

  x <- as.matrix(x)

  # as.matrix() on a data frame holding any non-numeric column silently
  # coerces every column to character; median() would then fail deep
  # inside apply() with an opaque message. Checked here instead.
  if(!is.numeric(x))
    stop("Argument 'x' must be numeric.")

  if(!is.logical(robust) || length(robust) != 1L || is.na(robust))
    stop("Argument 'robust' must be a single non-missing logical value.")

  if(!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("Argument 'na.rm' must be a single non-missing logical value.")

  nCol <- ncol(x)

  # center and scale mirror base::scale, which accepts either a logical
  # flag or a ready-made numeric vector of values.
  .resolve <- function(value, name, fun, mat) {

    if(is.logical(value)) {

      if(length(value) != 1L || is.na(value))
        stop(gettextf("Argument '%s' must be a single non-missing logical value, or a numeric vector.",
                      name), domain = NA)

      if(!value)
        return(NULL)

      return(apply(mat, 2, fun, na.rm = na.rm))

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

  centerFun <- if(robust) median else mean

  centerVals <- .resolve(center, "center", centerFun, x)

  if(!is.null(centerVals))
    x <- sweep(x, 2L, centerVals, "-", check.margin = FALSE)

  # The scale is taken from different data in the two branches, which is
  # why centering happens in between. The MAD is invariant to location, so
  # the robust factors are the same either way and are read off the
  # original columns, keeping "scaled:scale" independent of 'center'. The
  # root mean square is not invariant, and base::scale computes it after
  # centering, so the conventional branch does the same - which is what
  # makes it equal the standard deviation when 'center' is TRUE.
  scaleFun <- if(robust)
    mad
  else
    function(z, na.rm) {

      if(na.rm)
        z <- z[!is.na(z)]

      sqrt(sum(z^2) / max(1L, length(z) - 1L))

    }

  scaleVals <- .resolve(scale, "scale", scaleFun, x)

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
