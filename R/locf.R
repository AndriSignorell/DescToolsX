
#' Last Observation Carried Forward 
#' 
#' In longitudinal studies it's common that individuals drop out before all
#' responses can be obtained. Measurements obtained before the individual
#' dropped out can be used to impute the unknown measurement(s). The last
#' observation carried forward method is one way to impute values for the
#' missing observations. For the last observation carried forward (LOCF)
#' approach the missing values are replaced by the last observed value of that
#' variable for each individual regardless of when it occurred.
#' 
#' \code{locf()} replaces \code{NA}s with the most recent non-NA prior to it.
#' 
#' The function will replace all NAs found in a vector with the last earlier
#' value not being NA. In data.frames each column will be treated as described.
#' 
#' It should be noted, that the last observation carried forward approach may
#' result in biased estimates and may underestimate the variability.
#' 
#' @name locf
#' @param x a vector, a data.frame or a matrix containing NAs.
#'  
#' @return a vector with the same dimension as x.
#' 
#' @author Daniel Wollschlaeger <dwoll@@psychologie.uni-kiel.de>
#' 
#' @seealso See also the package \pkg{Hmisc} for less coarse imputation
#' functions.
#' 
#' @examples
#' 
#' d.frm <- data.frame(
#'   day=rep(c("mon", "tue", "wed", "thu", "fri", "sat", "sun"), 4)
#' , val=rep(c(runif(5), rep(NA,2)), 4) )
#' 
#' d.frm$locf <- locf( d.frm$val )
#' d.frm


#' @family imputation
#' @concept missing-data
#' @concept data-manipulation
#' @concept time-series
#'
#'
#' @export
locf <- function(x) {
  
  # last observation carried forward
  # replaces NAs by the last observed value
  
  #   while(any(is.na(x))) {
  #     x[is.na(x)] <- x[which(is.na(x))-1]
  #   }
  #   return(x)
  
  # faster solution from Daniel Wollschlaeger:
  
  # corrected by 0.99.19, as this didn't handle c(NA, 3.0, NA, 5,5) correctly
  # rep(x[!is.na(x)], diff(c(which(!is.na(x)), length(x)+1)))
  
  ok <- !is.na(x)
  rep(c(NA, x[ok]), diff(c(1L, which(ok), length(x) + 1L)))
  
}

