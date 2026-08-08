
#' Harmonic Mean and Its Confidence Interval 
#' 
#' Calculates the harmonic mean and its confidence interval of a vector x. 
#' 
#' 
#' To compute the harmonic mean, \code{1/x} is first calculated, before the
#' arithmetic mean and its confidence interval are computed by
#' \code{\link[lumen]{meanCI}}. The harmonic mean is then the reciprocal of the
#' arithmetic mean of the reciprocals of the values. The same applies to the
#' confidence interval.
#' 
#' The harmonic mean is restricted to strictly positive inputs. Non-positive
#' values are turned into \code{NA} and therefore make the result \code{NA}
#' unless \code{na.rm = TRUE}, in which case they are dropped. If the lower
#' bound of the confidence interval is not greater than zero, then the
#' confidence interval is not defined, and thus \code{NA} will be reported.
#'
#' \code{sides} names the side on which the finite bound lies: \code{"left"}
#' yields an interval bounded below, \code{"right"} one bounded above. The
#' harmonic mean of positive values is itself positive, so the open lower
#' side is reported as 0 rather than as \code{NA} or \eqn{-\infty}.
#' 
#' Use \code{\link{sapply}} to calculate the measures from data frame, resp.
#' from a matrix. \cr
#' 
#' @name hmean
#' 
#' @param x a positive numeric vector. An object which is not a vector is
#' coerced, if possible, by \code{as.vector()}.
#' @param conf.level confidence level of the interval. Default is \code{NA}. 
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}. You can specify just the initial letter. \code{"left"} would
#' be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param method a vector of character strings representing the type of
#' intervals required. The value should be any subset of the values
#' \code{"classic"}, \code{"boot"}.  See \code{\link[boot]{boot.ci}}. 
#' @param na.rm logical, indicating whether \code{NA} values should be stripped
#' before the computation proceeds. Defaults to \code{FALSE}. 
#' @param ... further arguments are passed to the \code{\link[boot]{boot}}
#' function. Supported arguments are \code{type} (\code{"norm"},
#' \code{"basic"}, \code{"stud"}, \code{"perc"}, \code{"bca"}), \code{parallel}
#' and the number of bootstrap replicates \code{R}. If not defined those will
#' be set to their defaults, being \code{"basic"} for \code{type}, option
#' \code{"boot.parallel"} (and if that is not set, \code{"no"}) for
#' \code{parallel} and \code{999} for \code{R}.
#' 
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of the harmonic mean}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#' 
#' @references Snedecor, G. W., Cochran, W. G. (1989) Statistical Methods, 8th
#' ed. Ames, \emph{IA: Iowa State University Press }
#' @examples
#' 
#' x <- runif(5)
#' hmean(x)
#' 
#' m <- matrix(runif(50), nrow = 10)
#' apply(m, 2, hmean)
#' 
#' sapply(as.data.frame(m), hmean)
#' 

#' @rdname hmean
#' @family location
#' @concept location
#' @concept nonlinear-mean
#' @export
hmean <- function(x, conf.level = NA, 
                  sides = c("two.sided","left","right"), method = c("classic", "boot"),
                  na.rm = FALSE, ...) {
  
  # see also for alternative ci
  # https://www.unistat.com/guide/confidence-intervals/
  
  is.na(x) <- x <= 0
  
  if(is.na(conf.level))
    res <- 1 / mean(1/x, na.rm = na.rm)
  
  else {
    #   res <- (1 / meanCI(x = 1/x, method = method,
    #                      conf.level = conf.level, sides = sides, na.rm=na.rm, ...))
    #
    #   if(!is.na(conf.level)){
    #     res[2:3] <- c(min(res[2:3]), max(res[2:3]))
    #     if(res[2] < 0)
    #       res[c(2,3)] <- NA
    #   }
    #
    
    sides <- match.arg(sides, choices = c("two.sided", "left",
                                          "right"), several.ok = FALSE)
    if (sides != "two.sided")
      conf.level <- 1 - 2 * (1 - conf.level)
    
    res <- (1/(mci <- meanCI(x = 1/x, method = method, conf.level = conf.level,
                             sides = "two.sided", na.rm = na.rm, ...)))[c(1, 3, 2)]
    
    # check if lower ci < 0, if so return NA, as CI not defined see Stata definition
    if( mci[2] <= 0) 
      res[2:3] <- NA
    
    names(res) <- c("est", "lci", "uci")
    
    if (sides == "left")
      res[3] <- Inf
    else if (sides == "right")
      # 0, not NA: the harmonic mean of positive values is positive, so
      # the open lower side has a boundary. NA claims the bound is unknown
      # (design_rules.md 4.1). gmean() arrives at the same 0 by way of
      # exp(-Inf).
      res[2] <- 0
    
  }
  
  return(res)
  
}
