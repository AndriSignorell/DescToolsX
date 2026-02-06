
#' Hodges-Lehmann Estimator of Location
#' 
#' Function to compute the Hodges-Lehmann estimator of location in the one and
#' two sample case following a clever fast algorithm by John Monahan (1984).
#' 
#' The Hodges-Lehmann estimator is the median of the combined data points and
#' Walsh averages.  It is the same as the Pseudo Median returned as a
#' by-product of the function \code{\link{wilcox.test}} (which however does not
#' calculate correctly as soon as ties are present).\cr Note that in the
#' two-sample case the estimator for the difference in location parameters does
#' not estimate the difference in medians (a common misconception) but rather
#' the median of the difference between a sample from x and a sample from y.
#' 
#' (The calculation of the confidence intervals is not yet implemented.)
#' 
#' @param x a numeric vector.
#' @param y an optional numeric vector of data values: as with x non-finite
#' values will be omitted.
#' @param conf.level confidence level of the interval.
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}.  You can specify just the initial letter. \code{"left"}
#' would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param method only bootstrap method is implemented so far. Others might be
#' implemented in future.
#' @param na.rm logical. Should missing values be removed? Defaults to
#' \code{FALSE}.
#' @param \dots the dots are passed to the function \code{\link[boot]{boot}},
#' when confidence intervals are calculated.
#' @return the Hodges-Lehmann estimator of location as a single numeric value
#' if no confidence intervals are requested,\cr and otherwise a numeric vector
#' with 3 elements for the estimate, the lower and the upper confidence
#' interval
#' @author Cyril Flurin Moser (Cyril did the lion's share and coded Monahan's
#' algorithm in C++), Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{wilcox.test}}, \code{\link{median}},
#' \code{\link{medianCI}}
#' @references Hodges, J.L., and Lehmann, E.L. (1963), Estimates of location
#' based on rank tests. \emph{The Annals of Mathematical Statistics},
#' \bold{34}, 598--611.
#' 
#' Monahan, J. (1984), Algorithm 616: Fast Computation of the Hodges-Lehmann
#' Location Estimator, \emph{ACM Transactions on Mathematical Software}, Vol.
#' 10, No. 3, pp. 265-270
#' @keywords nonparametric robust univar
#' @examples
#' 
#' set.seed(1)
#' x <- rt(100, df = 3)
#' y <- rt(100, df = 5)
#' 
#' hodgesLehmann(x)
#' hodgesLehmann(x, y)
#' 
#' # same as
#' wilcox.test(x, conf.int = TRUE)$estimate
#' 



#' @export
hodgesLehmann <- function(x, y = NULL, conf.level = NA, sides = c("two.sided","left","right"), 
                          method = c("boot"), na.rm = FALSE, ...) {
  
  #   Werner Stahel's version:
  #
  #   f.HodgesLehmann <- function(data)
  #   {
  #     ## Purpose:   Hodges-Lehmann estimate and confidence interval
  #     ## -------------------------------------------------------------------------
  #     ## Arguments:
  #     ## Remark: function changed so that CI covers >= 95%, before it was too
  #     ##         small (9/22/04)
  #     ## -------------------------------------------------------------------------
  #     ## Author: Werner Stahel, Date: 12 Aug 2002, 14:13
  #     ## Update: Beat Jaggi, Date: 22 Sept 2004
  #     .cexact <-
  #       # c(NA,NA,NA,NA,NA,21,26,33,40,47,56,65,74,84,95,107,119,131,144,158)
  #       c(NA,NA,NA,NA,NA,22,27,34,41,48,57,66,75,85,96,108,120,132,145,159)
  #     .d <- na.omit(data)
  #     .n <- length(.d)
  #     .wa <- sort(c(outer(.d,.d,"+")/2)[outer(1:.n,1:.n,"<=")])
  #     .c <- if (.n<=length(.cexact)) .n*(.n+1)/2+1-.cexact[.n] else
  #       floor(.n*(.n+1)/4-1.96*sqrt(.n*(.n+1)*(2*.n+1)/24))
  #     .r <- c(median(.wa), .wa[c(.c,.n*(.n+1)/2+1-.c)])
  #     names(.r) <- c("estimate","lower","upper")
  #     .r
  #   }
  
  if(na.rm) {
    if(is.null(y))
      x <- na.omit(x)
    else {
      ok <- complete.cases(x, y)
      x <- x[ok]
      y <- y[ok]
    }
  }
  
  if(anyNA(x) || (!is.null(y) && anyNA(y)))
    if(is.na(conf.level))
      return(NA)
  else
    return(c(est=NA,  lwr.ci=NA, upr.ci=NA))
  
  
  #  res <- wilcox.test(x,  y, conf.int = TRUE, conf.level = Coalesce(conf.level, 0.8))
  if(is.null(y)){
    res <- hlqest(x)
  } else {
    res <- hl2qest(x, y)
  }
  
  if(is.na(conf.level)){
    result <-  res
    names(result) <- NULL
    
  } else {
    
    sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)
    
    n <- length(x)
    
    
    if( method=="boot"){
      
      # boot arguments in dots ...
      # adjusted bootstrap percentile (BCa) interval
      btype <- inDots(..., arg="type", default="bca")
      R <- inDots(..., arg="R", default=999)
      parallel <- inDots(..., arg="parallel", default="no")
      ncpus <- inDots(..., arg="ncpus", default=getOption("boot.ncpus", 1L))
      
      
      # ToDo *******************
      # *******  implement here the two sample case!! ***********
      # ToDo *******************
      
      boot.fun <- boot::boot(x, 
                             function(x, d) hlqest(x[d]), 
                             R=R, parallel=parallel, ncpus=ncpus)
      ci <- boot::boot.ci(boot.fun, conf=conf.level, type=btype)
      
      if(btype == "norm"){
        res <- c(est=boot.fun$t0, lci=ci[[4]][2], uci=ci[[4]][3])
      } else {
        res <- c(est=boot.fun$t0, lci=ci[[4]][4], uci=ci[[4]][5])
      }
      
    } else {
      # we'll do that later down the road
      
      # lci <- n^2/2 + qnorm((1-conf.level)/2) * sqrt(n^2 * (2*n+1)/12) - 0.5
      # uci <- n^2/2 - qnorm((1-conf.level)/2) * sqrt(n^2 * (2*n+1)/12) - 0.5
      lci <- uci <- NA
      warning("Confidence intervals not yet implemented for Hodges-Lehman-Estimator.")
      
    }
    
    if(sides=="left")        res[3] <- Inf
    else if(sides=="right")  res[2] <- -Inf
    
    result <- c(est=res,  lwr.ci=lci, upr.ci=uci)
  }
  
  return(result)
  
}

