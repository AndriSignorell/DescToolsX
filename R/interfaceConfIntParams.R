
#' Common arguments for confidence interval functions.
#'
#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned.
#'
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See details in [ConfidenceIntervals()].
#'
#' @param method character string specifying the interval method. Common
#'   choices are `"classic"` and `"boot"`; the available methods
#'   and their defaults depend on the function. 
#'   
#' @param ... further arguments passed to the bootstrap engine when
#'   `method = "boot"`: `R`, `type`, `parallel`,
#'   `ncpus`. 
#'   
#' @noRd   
NULL
