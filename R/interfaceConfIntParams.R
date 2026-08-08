
#' Common arguments for confidence interval functions.
#'
#' @param conf.level confidence level of the interval. If set to \code{NA}
#'   (the default), only the point estimate is returned.
#'
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of \code{"two.sided"} (default), \code{"left"} or
#'   \code{"right"}). See details in \code{\link{ConfidenceIntervals}}.
#'
#' @param method character string specifying the interval method. Common
#'   choices are \code{"classic"} and \code{"boot"}; the available methods
#'   and their defaults depend on the function. 
#'   
#' @param ... further arguments passed to the bootstrap engine when
#'   \code{method = "boot"}: \code{R}, \code{type}, \code{parallel},
#'   \code{ncpus}. 
#'   
#' @noRd   
NULL