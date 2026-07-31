
#' Compute Area Under the Curve (auc)
#'
#' Calculates the area under a curve defined by points \code{(x, y)} using
#' different numerical integration methods.
#'
#' @param x,y numeric vectors of equal length defining the curve coordinates
#' @param from,to numeric values specifying the integration interval. Defaults to
#'   the range of \code{x}.
#' @param method character string specifying the integration method:
#'   \code{"trapezoid"} (default), \code{"step"}, or \code{"spline"}
#' @param absoluteArea logical; if \code{TRUE}, areas below zero are treated
#'   as positive
#' @param subdivisions integer; number of subdivisions for spline integration
#' @param na.rm logical; if \code{TRUE}, incomplete \code{(x, y)} pairs are
#'   removed before computation. If \code{FALSE} and missing values are
#'   present, \code{NA_real_} is returned.
#' @param ... additional arguments passed to \code{\link[stats]{approx}}
#'
#' @return a numeric value representing the computed area under the curve
#'
#' @details
#' The function supports three methods:
#' \itemize{
#'   \item \strong{trapezoid}: Linear interpolation between points.
#'   \item \strong{step}: Step function using left endpoints.
#'   \item \strong{spline}: Smooth interpolation using cubic splines.
#' }
#'
#' If \code{absoluteArea = TRUE}, the function accounts for sign changes by
#' inserting zero-crossings and summing absolute areas.
#'
#' \code{from} and \code{to} must lie inside the range of \code{x} for the
#' interpolating methods: outside it there is nothing to interpolate, and
#' \code{\link[stats]{approx}} would return \code{NA} for those points and
#' hence an \code{NA} area. Extrapolation is not attempted.
#'
#' @examples
#' x <- c(1, 2, 3, 5)
#' y <- c(0, 1, 1, 2)
#'
#' auc(x, y)
#' auc(x, y, method = "step")
#' auc(x, y, method = "spline")
#' auc(x, y, absoluteArea = TRUE)
#'
#' @seealso \code{\link[stats]{approx}}, \code{\link[stats]{splinefun}},
#'   \code{\link[stats]{integrate}}
#'
#' @family model.metrics
#' @concept model-evaluation
#' @concept roc
#' @export
auc <- function(x, y, from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE),
                method = c("trapezoid", "step", "spline"), absoluteArea = FALSE,
                subdivisions = 100, na.rm = FALSE, ...) {

  # calculates area under the curve
  # example:
  #   auc(x = c(1, 2, 3, 5), y = c(0, 1, 1, 2))
  #   auc(x = c(2, 3, 4, 5), y = c(0, 1, 1, 2))

  method <- match.arg(method)

  if (length(x) != length(y))
    stop("length x must equal length y")

  if (na.rm) {
    idx <- complete.cases(cbind(x, y))
    x <- x[idx]
    y <- y[idx]

  } else if (anyNA(x) || anyNA(y)) {
    # approx() defaults to na.rm = TRUE and would have dropped the missing
    # pairs silently, while splinefun() errors - so na.rm = FALSE meant
    # three different behaviours for three methods. Be explicit instead.
    return(NA_real_)
  }

  if (length(x) < 2)
    return(NA_real_)

  o <- order(x)
  x <- x[o]
  y <- y[o]

  # 'from'/'to' are promises evaluated here for the first time, i.e. after
  # the NA removal and sorting above - force them before they are used so
  # the checks below see the same values the integration will.
  force(from)
  force(to)

  if (!is.numeric(from) || length(from) != 1L || !is.finite(from) ||
      !is.numeric(to) || length(to) != 1L || !is.finite(to))
    stop("'from' and 'to' must be single finite numbers")

  if (from > to)
    stop("'from' must not be greater than 'to'")

  if (method != "spline" && (from < min(x) || to > max(x)))
    stop("'from' and 'to' must lie within the range of 'x'")

  # inserts the zero-crossings of the polyline so that sign changes are
  # split into separate, individually positive pieces
  .addZeroCrossings <- function(x, y) {
    idx <- which(diff(y >= 0) != 0)
    if (length(idx) == 0L)
      return(list(x = x, y = y))
    cross <- x[idx] - y[idx] * (x[idx + 1L] - x[idx]) / (y[idx + 1L] - y[idx])
    list(x = c(x, cross), y = c(y, rep(0, length(idx))))
  }

  if (method == "trapezoid" || method == "step") {

    if (absoluteArea) {
      z <- .addZeroCrossings(x, y)
    } else {
      z <- list(x = x, y = y)
    }

    xout <- sort(unique(c(from, to, z$x[z$x > from & z$x < to])))
    values <- approx(z$x, z$y, xout = xout, ...)

    yv <- if (absoluteArea) abs(values$y) else values$y

    res <- if (method == "trapezoid")
      0.5 * sum(diff(values$x) * (yv[-1L] + yv[-length(yv)]))
    else
      sum(diff(values$x) * yv[-length(yv)])

  } else {

    sf <- splinefun(x, y, method = "natural")
    myfunction <- if (absoluteArea) function(z) abs(sf(z)) else sf

    res <- integrate(myfunction, lower = from, upper = to,
                     subdivisions = subdivisions)$value
  }

  return(res)
}
