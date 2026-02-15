
#' Pearson's Contingency Coefficient
#' 
#' Calculate Pearson's contingency coefficient of \code{x}, if \code{x} is a table. If both,
#' \code{x} and \code{y} are given, then the according table will be built
#' first (see \link{Association}).
#' The contingency coefficient goes from 0 to \eqn{\sqrt(\frac{min(r, c) - 1}{min(r, c)})}. For
#' the corrected contingency coefficient the range is 0 to 1. 
#' 
#' @aliases ContCoef
#' @inheritParams Association
#' @param method string defining the method to calculate confidence intervals
#' for the contingency coefficient. Only \code{"boot"} implemented.
#' 
#' @param correct logical (default \code{FALSE}), indicates, whether
#' Sakoda's adjusted Pearson's C should be returned. 
#' 
#' @return a single numeric value if no confidence intervals are requested,\cr
#' and otherwise a numeric vector with 3 elements for the estimate, the lower
#' and the upper confidence interval. 
#' 
#' @details
#' For Pearson’s contingency coefficient 
#' no generally accepted analytical confidence intervals exist. If interval 
#' estimation is required, resampling methods such as the bootstrap may be 
#' applied. In applied research, effect size measures with better inferential 
#' properties (e.g. Cramér’s V) are usually preferred.
#' 
#' @author Andri Signorell <andri@@signorell.net>,
#' @seealso \code{\link{Association}}, \code{\link{cramerV}}
#' \code{\link{pairApply}}
#' Sakoda, J.M. (1977) Measures of Association for Multivariate Contingency
#' Tables, \emph{Proceedings of the Social Statistics Section of the American
#' Statistical Association} (Part III), 777-780.
#' 
#' @keywords multivariate
#' @examples
#' 
#' tab <- apply(HairEyeColor, c(1,2), sum)
#' contCoef(tab)
#' 
#' # just x and y
#' with(untable(tab), contCoef(Hair, Eye))
#' 


# Pearson's Contingency Coefficient
#' @export
contCoef <- function(x, y = NULL, conf.level = NA,
                     method = "boot",
                     correct = FALSE, ...) {
  
  tab <- .normalizeToConfusion(x, y, mode = "association")
  
  chisq <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  cc <- as.numeric( sqrt( chisq / ( chisq + sum(tab)) ))
  
  if(correct) {  # Sakoda's adjusted Pearson's C
    k <- min(nrow(tab), ncol(tab))
    cc <- cc / sqrt((k-1) / k)
  }
  
  if(!is.na(conf.level))
    # **** ToDo ****: boot ci
    a <- method 
  
  return(cc)
  
}

