
#' Compute Area Under the Curve
#'
#' Calculates the area under a curve defined by points `(x, y)` using
#' different numerical integration methods.
#'
#' @param x,y numeric vectors of equal length defining the curve coordinates
#' @param from,to single numeric values specifying the integration interval;
#'   by default, the range of `x`
#' @param method character string specifying the integration method:
#'   `"trapezoid"`, `"step"`, or `"spline"`
#' @param absoluteArea logical; whether areas below zero are counted as positive
#' @param subdivisions positive whole number specifying the maximum number of
#'   subdivisions used for spline integration
#' @param na.rm logical; whether incomplete `(x, y)` pairs are removed
#' @param ... additional arguments passed to [stats::approx()] for
#'   trapezoidal interpolation
#'
#' @return a numeric value representing the computed area
#'
#' @details
#' The available methods are:
#' \itemize{
#'   \item `"trapezoid"`: linear interpolation between successive points
#'   \item `"step"`: a right-continuous step function using the value at
#'     the left endpoint of each interval
#'   \item `"spline"`: a natural cubic spline integrated numerically
#' }
#'
#' For `method = "step"`, an integration boundary lying between two
#' observed `x` values retains the value of the preceding point. No
#' linear interpolation is performed at the boundary.
#'
#' If `absoluteArea = TRUE`, the absolute value of the interpolated curve
#' is integrated. Sign changes in linear segments are split at their exact
#' zero-crossing. For the step method, the absolute values of the constant
#' step heights are used.
#'
#' Both integration limits must lie inside the range of `x`.
#' Extrapolation is not performed.
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
#' # interval boundaries between observed x values
#' auc(
#'   x = c(0, 1, 2),
#'   y = c(-2, 10, 4),
#'   from = 0.5,
#'   to = 1.5,
#'   method = "step"
#' )
#'
#' auc(
#'   x = c(0, 1, 2),
#'   y = c(-2, 10, 4),
#'   from = 0.5,
#'   to = 1.5,
#'   method = "step",
#'   absoluteArea = TRUE
#' )
#'
#' @seealso [stats::approx()], [stats::splinefun()],
#'   [stats::integrate()]
#'
#' @family model.metrics
#' @concept model-evaluation
#' @concept roc
#' @export
auc <- function(x, y, from = min(x, na.rm = TRUE),
                to = max(x, na.rm = TRUE),
                method = c("trapezoid", "step", "spline"),
                absoluteArea = FALSE, subdivisions = 100,
                na.rm = FALSE, ...) {
  
  method <- match.arg(method)
  
  if (!is.numeric(x) || !is.numeric(y))
    stop("'x' and 'y' must be numeric vectors")
  
  if (length(x) != length(y))
    stop("'x' and 'y' must have equal lengths")
  
  if (!is.logical(absoluteArea) || length(absoluteArea) != 1L ||
      is.na(absoluteArea))
    stop("'absoluteArea' must be TRUE or FALSE")
  
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("'na.rm' must be TRUE or FALSE")
  
  if (method == "spline" &&
      (!is.numeric(subdivisions) || length(subdivisions) != 1L ||
       !is.finite(subdivisions) || subdivisions < 1 ||
       subdivisions != floor(subdivisions)))
    stop("'subdivisions' must be a single positive whole number")
  
  if (na.rm) {
    keep <- complete.cases(x, y)
    x <- x[keep]
    y <- y[keep]
    
  } else if (anyNA(x) || anyNA(y)) {
    return(NA_real_)
  }
  
  if (length(x) < 2L)
    return(NA_real_)
  
  if (any(!is.finite(x)) || any(!is.finite(y)))
    stop("'x' and 'y' must contain finite values")
  
  o <- order(x)
  x <- x[o]
  y <- y[o]
  
  if (anyDuplicated(x))
    stop("'x' values must be unique")
  
  force(from)
  force(to)
  
  if (!is.numeric(from) || length(from) != 1L || !is.finite(from))
    stop("'from' must be a single finite number")
  
  if (!is.numeric(to) || length(to) != 1L || !is.finite(to))
    stop("'to' must be a single finite number")
  
  if (from > to)
    stop("'from' must not be greater than 'to'")
  
  if (from < x[1L] || to > x[length(x)])
    stop("'from' and 'to' must lie within the range of 'x'")
  
  if (from == to)
    return(0)
  
  knots <- c(
    from,
    x[x > from & x < to],
    to
  )
  
  if (method == "step") {
    
    left <- knots[-length(knots)]
    idx <- findInterval(left, x)
    heights <- y[idx]
    
    if (absoluteArea)
      heights <- abs(heights)
    
    res <- sum(diff(knots) * heights)
    
  } else if (method == "trapezoid") {
    
    values <- approx(x, y, xout = knots, ...)$y
    
    yLeft  <- values[-length(values)]
    yRight <- values[-1L]
    widths <- diff(knots)
    
    if (!absoluteArea) {
      
      areas <- widths * (0.5 * yLeft + 0.5 * yRight)
      
    } else {
      
      a <- abs(yLeft)
      b <- abs(yRight)
      
      areas <- widths * (0.5 * a + 0.5 * b)
      
      crossing <- (yLeft < 0 & yRight > 0) |
        (yLeft > 0 & yRight < 0)
      
      if (any(crossing)) {
        
        ac <- a[crossing]
        bc <- b[crossing]
        
        scale <- pmax(ac, bc)
        fraction <- (ac / scale) /
          ((ac / scale) + (bc / scale))
        
        areas[crossing] <-
          0.5 * widths[crossing] *
          (ac * fraction + bc * (1 - fraction))
      }
    }
    
    res <- sum(areas)
    
  } else {
    
    splineFunction <- splinefun(x, y, method = "natural")
    
    integrand <- if (absoluteArea) {
      function(z) abs(splineFunction(z))
    } else {
      splineFunction
    }
    
    res <- integrate(
      integrand,
      lower = from,
      upper = to,
      subdivisions = subdivisions
    )$value
  }
  
  return(res)
}


