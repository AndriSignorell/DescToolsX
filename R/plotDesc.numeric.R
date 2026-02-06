#' Plot method for Desc.numeric objects
#'
#' Visualizes the distribution of a numeric \code{Desc} object.
#' Depending on the internal implementation, the plot may include
#' a histogram, density curve, boxplot and empirical distribution.
#'
#' @param x An object of class \code{"Desc.numeric"}.
#' @param ... Further graphical parameters passed to the underlying
#'   base R plotting functions.
#' 
#' @aliases plot.Desc
#' @details
#' This function is an S3 method for \code{\link[graphics]{plot}}.
#' It is automatically dispatched when calling \code{plot(x)} on a
#' \code{Desc.numeric} object.
#'
#' Named colors defined by \code{DescToolsX} (e.g. \code{"hred"},
#' \code{"hblue"}) can be used directly.
#'
#' @return
#' Invisibly returns \code{NULL}.
#'
#' @examples
#' x <- rnorm(100)


#' @method plot Desc
#' @export
plot.Desc <- function(x, ...) {
  stop("no plot method for this Desc object", call. = FALSE)
}
