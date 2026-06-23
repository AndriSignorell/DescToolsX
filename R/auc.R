
#' Compute Area Under the Curve (auc)
#'
#' Calculates the area under a curve defined by points \code{(x, y)} using
#' different numerical integration methods.
#'
#' @param x,y Numeric vectors of equal length defining the coordinates of the curve.
#' @param from,to Numeric values specifying the integration interval. Defaults to
#'   the range of \code{x}.
#' @param method Character string specifying the integration method:
#'   \code{"trapezoid"} (default), \code{"step"}, or \code{"spline"}.
#' @param absoluteArea Logical; if \code{TRUE}, the absolute area is computed,
#'   i.e., areas below zero are treated as positive.
#' @param subdivisions Integer; number of subdivisions for spline integration.
#' @param na.rm Logical; if \code{TRUE}, missing values are removed before computation.
#' @param ... Additional arguments passed to \code{\link{approx}}.
#'
#' @return A numeric value representing the computed area under the curve.
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
#' @examples
#' x <- c(1, 2, 3, 5)
#' y <- c(0, 1, 1, 2)
#'
#' auc(x, y)
#' auc(x, y, method = "step")
#' auc(x, y, method = "spline")
#' auc(x, y, absoluteArea = TRUE)
#'
#' @seealso \code{\link{approx}}, \code{\link{splinefun}}, \code{\link{integrate}}
#'


#' @family regression.utils
#' @concept regression
#' @concept classification-metrics
#' @concept descriptive-statistics
#'
#'
#' @export
auc <- function(x, y, from = min(x, na.rm=TRUE), to = max(x, na.rm=TRUE),
                method=c("trapezoid", "step", "spline"), absoluteArea = FALSE,
                subdivisions = 100,  na.rm = FALSE, ...)  {
  
  
  # calculates Area unter the curve
  # example:
  #   auc( x=c(1,2,3,5), y=c(0,1,1,2))
  #   auc( x=c(2,3,4,5), y=c(0,1,1,2))
  
  method <- match.arg(method)
  
  if(na.rm) {
    idx <- complete.cases(cbind(x,y))
    x <- x[idx]
    y <- y[idx]
  }
  
  if (length(x) != length(y))
    stop("length x must equal length y")
  
  if (length(x) < 2)
    return(NA)
  
  o <- order(x)
  x <- x[o]
  y <- y[o]
  
  ox <- x
  oy <- y
  
  
  if (method=="trapezoid") {
    
    # easy and short
    # , "trapezoid" = { a <- sum((apply( cbind(y[-length(y)], y[-1]), 1, mean))*(x[-1] - x[-length(x)])) }
    
    ## Default option
    if (!absoluteArea) {
      values <- approx(x, y, xout = sort(unique(c(from, to, x[x > from & x < to]))), ...)
      res <- 0.5 * sum(diff(values$x) * (values$y[-1] + values$y[-length(values$y)]))
      
    } else { ## Absolute areas
      
      idx <- which(diff(oy >= 0)!=0)
      newx <- c(x, x[idx] - oy[idx]*(x[idx+1]-x[idx]) / (y[idx+1]-y[idx]))
      newy <- c(y, rep(0, length(idx)))
      values <- approx(newx, newy, xout = sort(unique(c(from, to, newx[newx > from & newx < to]))), ...)
      
      res <- 0.5 * sum(diff(values$x) * (abs(values$y[-1]) + abs(values$y[-length(values$y)])))
      
    }
    
  } else if (method=="step") {
    
    # easy and short
    # , "step" = { a <- sum( y[-length(y)] * (x[-1] - x[-length(x)])) }
    
    ## Default option
    if (!absoluteArea) {
      values <- approx(x, y, xout = sort(unique(c(from, to, x[x > from & x < to]))), ...)
      
      res <- sum(diff(values$x) * values$y[-length(values$y)])
      # res <- sum( y[-length(y)] * (x[-1] - x[-length(x)]))
      
    } else { ## Absolute areas
      
      idx <- which(diff(oy >= 0)!=0)
      newx <- c(x, x[idx] - oy[idx]*(x[idx+1]-x[idx]) / (y[idx+1]-y[idx]))
      newy <- c(y, rep(0, length(idx)))
      values <- approx(newx, newy, xout = sort(unique(c(from, to, newx[newx > from & newx < to]))), ...)
      
      res <- sum(diff(values$x) * abs(values$y[-length(values$y)]))
      
    }
    
  } else if (method=="spline") {
    
    if (absoluteArea)
      myfunction <- function(z) { abs(splinefun(x, y, method="natural")(z)) }
    else
      myfunction <- splinefun(x, y, method="natural")
    
    res <- integrate(myfunction, lower=from, upper=to, subdivisions=subdivisions)$value
    
  }
  
  return(res)
  
}
