

#' Skewness and Kurtosis
#' 
#' \code{skewX} computes the skewness, \code{kurtX} the excess kurtosis of the
#' values in x.
#' 
#' \code{kurtX()} returns the excess kurtosis, therefore the kurtosis calculates
#' as \code{kurtX(x) + 3} if required.
#' 
#' If \code{na.rm} is \code{TRUE} then missing values are removed before
#' computation proceeds. \cr
#' 
#' The estimator for calculating the skewness can either be:\cr \code{1: g_1 =
#' m_3 / m_2^(3/2) } \cr \code{2: G_1 = g_1 * sqrt(n(n-1)) / (n-2) }\cr
#' \code{3: b_1 = m_3 / s^3 = g_1 ((n-1)/n)^(3/2) } \cr
#' 
#' and the one for the kurtosis:\cr \code{1: g_2 = m_4 / m_2^2 - 3 } \cr
#' \code{2: G_2 = ((n+1) g_2 + 6) * (n-1) / ((n-2)(n-3)) } \cr \code{3: b_2 =
#' m_4 / s^4 - 3 = (g_2 + 3) (1 - 1/n)^2 - 3 } \cr
#' 
#' 1 is the typical definition used in Stata and in many older textbooks.  \cr
#' 2 is used in SAS and SPSS.  \cr 3 is used in MINITAB and BMDP. \cr
#' 
#' Cramer (1997) mentions the asymptotic standard error of the skewness, resp.
#' kurtosis: \cr \preformatted{ASE.skew = sqrt( 6*n*(n-1)/((n-2)*(n+1)*(n+3)) )
#' ASE.kurt = sqrt((24*n*(n - 1)^2) / ((n - 3)*(n - 2)*(n + 3)*(n + 5)))} to be
#' used for calculating the confidence intervals.  This is implemented here
#' with \code{method="classic"}. \cr However, Joanes and Gill (1998) advise
#' against this approach, pointing out that the normal assumptions would
#' virtually always be violated.  They suggest using the bootstrap method.
#' That's why the default method for the confidence interval type is set to
#' \code{"boot"}. If not further specified the boot ci type will be chosen as
#' \code{"bca"}.\cr
#' 
#' This implementation of the two functions is comparably fast, as the
#' expensive sums are coded in C.
#' 
#' @name skew_kurt
#' @aliases skewX kurtX
#' @param x a numeric vector. An object which is not a vector is coerced (if
#' possible) by \code{as.vector}.
#' @param conf.level confidence level of the interval. If set to \code{NA}
#' (which is the default) no confidence interval will be calculated.
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}. \code{"left"} would be analogue to a hypothesis of
#' \code{"greater"} in a \code{t.test}. You can specify just the initial
#' letter.
#' @param method a character string, defining the type of intervals required.
#' The value should be one out of \code{"classic"}, \code{"boot"} (default).
#' Further arguments for the boot function (as \code{R}, \code{type}, ...) can
#' be provided by the dots' argument if needed.
#' @param estimator integer, either 1, 2 or 3 (default) defining the algorithm
#' used for calculation. See Details.
#' @param weights a numerical vector of weights the same length as \code{x}
#' giving the weights to use for elements of \code{x}.
#' @param na.rm logical, indicating whether \code{NA} values should be stripped
#' before the computation proceeds. Defaults to \code{FALSE}.
#' @param \dots the dots are passed to the function \code{\link[boot]{boot}},
#' when confidence intervalls are calculated.
#' @return If \code{conf.level} is set to \code{NA} then the result will be
#' \item{a}{ single numeric value} and if a \code{conf.level} is provided, a
#' named numeric vector with 3 elements: \item{est}{the specific estimate,
#' either skewness or kurtosis} \item{lci}{lower bound of the confidence
#' interval} \item{uci}{upper bound of the confidence interval}
#' @author Andri Signorell <andri@@signorell.net>, David Meyer
#' <david.meyer@@r-project.org> (method = 3)
#' @seealso \code{\link{mean}}, \code{\link{sd}}, similar code in
#' \code{library(e1071)}
#' @references Cramer, D. (1997): \emph{Basic Statistics for Social Research}
#' Routledge.
#' 
#' Joanes, D. N., Gill, C. A. (1998): Comparing measures of sample skewness and
#' kurtosis. \emph{The Statistician}, 47, 183-189.
#' @keywords math
#' @examples
#' 
#' skewX(d.pizza$price, na.rm=TRUE)
#' kurtX(d.pizza$price, na.rm=TRUE)
#' 
#' # use sapply to calculate skewness for a data.frame
#' sapply(d.pizza[,c("temperature","price","delivery_min")], skewX, na.rm=TRUE)
#' 
#' # or apply to do that columnwise with a matrix
#' apply(as.matrix(d.pizza[,c("temperature","price","delivery_min")]), 2, skewX, na.rm=TRUE)
#' 



# skewX(
#   x,                               # 1) Data
#   conf.level = NA,                 # 2) CI-control
#   sides = c("two.sided", "left", "right"),
#   method = c("boot", "classic"),   # 3) CI-method
#   type = 3,                        # 4) Skewness-Definition
#   weights = NULL,
#   na.rm = FALSE,                   # 6) data sanity
#   ...


#' @rdname skew_kurt
#' @export
skewX <- function (x, 
                  conf.level = NA, sides=c("two.sided", "left", "right"), 
                  method = c("boot", "classic"), 
                  estimator = 3, weights=NULL, na.rm = FALSE, ...) {
  
  # C++ part for the expensive (x - mean(x))^2 etc. is roughly 14 times faster
  #   > x <- rchisq(100000000, df=2)
  #   > system.time(Skew(x))
  #   user  system elapsed
  #   6.32    0.30    6.62
  #   > system.time(Skew2(x))
  #   user  system elapsed
  #   0.47    0.00    0.47
  
  
  i.skew <- function(x, weights=NULL, estimator = 3) {
    
    # estimator 1: older textbooks
    if(!is.null(weights)){
      # use a standard treatment for weights
      z <- .NormWeights(x, weights, na.rm=na.rm, zero.rm=TRUE)
      r.skew <- rskeww_cpp(as.numeric(z$x), as.numeric(meanX(z$x, weights = z$weights)), 
                       as.numeric(z$weights))
      n <- z$wsum
      
    } else {
      if (na.rm) x <- na.omit(x)
      r.skew <- rskew_cpp(as.numeric(x), as.numeric(mean(x)))
      n <- length(x)
      
    }
    
    se <- sqrt((6*(n-2))/((n+1)*(n+3)))
    
    if (estimator == 2) {
      # estimator 2: SAS/SPSS
      r.skew <- r.skew * n^0.5 * (n - 1)^0.5/(n - 2)
      se <- se * sqrt(n*(n-1))/(n-2)
    }
    else if (estimator == 3) {
      # estimator 3: MINITAB/BDMP
      r.skew <- r.skew * ((n - 1)/n)^(3/2)
      se <- se * ((n - 1)/n)^(3/2)
    }
    
    return(c(r.skew, se^2))
  }
  
  
  
  
  
  if(is.na(conf.level)){
    res <- i.skew(x, weights=weights, estimator=estimator)[1]
    
  } else {
    
    sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                       several.ok = FALSE)
    
    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)
    
    
    if(method == "classic") {
      res <- i.skew(x, weights=weights, estimator=estimator)
      res <- c(est=res[1],
               lci=qnorm((1-conf.level)/2) * sqrt(res[2]),
               uci=qnorm(1-(1-conf.level)/2) * sqrt(res[2]))
      
    } else {
      # Problematic standard errors and confidence intervals for skewness and kurtosis.
      # Wright DB, Herrington JA. (2011) recommend only bootstrap intervals
      # adjusted bootstrap percentile (BCa) interval
      
      # boot arguments in dots ...
      btype <- inDots(..., arg="type", default="bca")
      R <- inDots(..., arg="R", default=999)
      parallel <- inDots(..., arg="parallel", default="no")
      ncpus <- inDots(..., arg="ncpus", default=getOption("boot.ncpus", 1L))
      
      boot.fun <- boot::boot(x, function(x, d) 
        i.skew(x[d], weights=weights, estimator=estimator), 
        R=R, parallel=parallel, ncpus=ncpus)
      ci <- boot::boot.ci(boot.fun, conf=conf.level, type=btype)
      
      
      if(method == "norm"){
        res <- c(est=boot.fun$t0, lci=ci[[4]][2], uci=ci[[4]][3])
      } else {
        res <- c(est=boot.fun$t0, lci=ci[[4]][4], uci=ci[[4]][5])
      }
      
    }
    
    if(sides=="left")
      res[3] <- Inf
    else if(sides=="right")
      res[2] <- -Inf
    
  }
  
  return(res)
  
}



#' @rdname skew_kurt
#' @export
kurtX <- function (x, 
                  conf.level = NA, sides=c("two.sided", "left", "right"), 
                  method = c("boot", "classic"), 
                  estimator = 3, weights=NULL, na.rm = FALSE, ...) {
  
  
  i.kurt <- function(x, weights=NULL, na.rm = FALSE, estimator = 3) {
    
    # estimator 1: older textbooks
    if(!is.null(weights)){
      # use a standard treatment for weights
      z <- .NormWeights(x, weights, na.rm=na.rm, zero.rm=TRUE)
      r.kurt <- rkurtw_cpp(as.numeric(z$x), 
                       as.numeric(meanX(z$x, weights = z$weights)), 
                       as.numeric(z$weights))
      n <- z$wsum
      
    } else {
      if (na.rm) x <- na.omit(x)
      r.kurt <- rkurt_cpp(as.numeric(x), 
                      as.numeric(mean(x)))
      n <- length(x)
      
    }
    
    se <- sqrt((24*n*(n-2)*(n-3))/((n+1)^2*(n+3)*(n+5)))
    #    se <- sqrt((24 * n * (n - 1)^2) / ((n - 3) * (n - 2) * (n + 3) * (n + 5)))
    
    if (estimator == 2) {
      # estimator 2: SAS/SPSS
      r.kurt <- ((r.kurt + 3) * (n + 1)/(n - 1) - 3) * (n - 1)^2/(n - 2)/(n - 3)
      se <- se * (((n-1)*(n+1))/((n-2)*(n-3)))
    }
    else if (estimator == 3) {
      # estimator 3: MINITAB/BDMP
      r.kurt <- (r.kurt + 3) * (1 - 1/n)^2 - 3
      se <- se * ((n-1)/n)^2
    }
    return(c(r.kurt, se^2))
  }
  
  
  if(is.na(conf.level)){
    res <- i.kurt(x, weights=weights, na.rm=na.rm, estimator=estimator)[1]
    
  } else {
    
    sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                       several.ok = FALSE)
    
    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)
    
    
    if(method == "classic") {
      
      res <- i.kurt(x, weights=weights, estimator=estimator)
      res <- c(
        est = res[1],
        lci = qnorm((1-conf.level)/2) * sqrt(res[2]),
        uci = qnorm(1-(1-conf.level)/2) * sqrt(res[2])) 
      
    } else {
      # Problematic standard errors and confidence intervals for skewness and kurtosis.
      # Wright DB, Herrington JA. (2011) recommend only bootstrap intervals
      # adjusted bootstrap percentile (BCa) interval
      
      # boot arguments in dots ...
      btype <- inDots(..., arg="type", default="bca")
      R <- inDots(..., arg="R", default=999)
      parallel <- inDots(..., arg="parallel", default="no")
      ncpus <- inDots(..., arg="ncpus", default=getOption("boot.ncpus", 1L))
      
      boot.fun <- boot::boot(x, function(x, d) 
        i.kurt(x[d], weights=weights, estimator=estimator), 
        R=R, parallel=parallel, ncpus=ncpus)
      ci <- boot::boot.ci(boot.fun, conf=conf.level, type=btype)
      
      
      if(method == "norm"){
        res <- c(est=boot.fun$t0, lci=ci[[4]][2], uci=ci[[4]][3])
      } else {
        res <- c(est=boot.fun$t0, lci=ci[[4]][4], uci=ci[[4]][5])
      }
      
    }
    
    if(sides=="left")
      res[3] <- Inf
    else if(sides=="right")
      res[2] <- -Inf
    
  }
  
  return(res)
  
}
























