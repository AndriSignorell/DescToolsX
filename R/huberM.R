
#' Safe (generalized) Huber M-Estimator of Location
#' 
#' (Generalized) Huber M-estimator of location with MAD scale, being sensible
#' also when the scale is zero where \code{\link[MASS]{huber}()} returns an
#' error.
#' 
#' The standard error is computed using the \eqn{\tau} correction factor but no
#' finite sample correction.\cr The original function is not exported, but can
#' be accessed as \code{DescToolsX::.huberM}.  % and as if \code{s} was not
#' estimated from the data.
#' 
#' @param x numeric vector.
#' @param conf.level confidence level of the interval. If set to \code{NA}
#' (which is the default) no confidence interval will be calculated.
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}.  You can specify just the initial letter. \code{"left"}
#' would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param method The type of confidence interval required. The value should be
#' any subset of the values \code{"wald"}, \code{"boot"}.
#' @param k positive factor; the algorithm winsorizes at \code{k} standard
#' deviations.
#' @param mu initial location estimator.
#' @param s scale estimator held constant through the iterations.
#' @param na.rm logical, indicating whether \code{NA} values should be stripped
#' before the computation proceeds. Defaults to FALSE.
#' @param \dots the dots are passed to the function
#' \code{\link[boot]{boot.ci}}, when confidence intervalls are calculated.
#' @return If \code{conf.level} is set to \code{NA} then the result will be
#' \item{a}{ single numeric value} and if a \code{conf.level} is provided, a
#' named numeric vector with 3 elements: \item{est}{the estimate for location}
#' \item{lci}{lower bound of the confidence interval} \item{uci}{upper bound of
#' the confidence interval}
#' @author Martin Maechler, building on the MASS code mentioned.\cr Andri
#' Signorell <andri@@signorell.net> (confidence intervals and interface)
#' @seealso \code{\link[MASS]{hubers}} (and \code{huber}) in package
#' \pkg{MASS}; \code{\link{mad}}.
#' @references Huber, P. J. (1981) \emph{Robust Statistics.} Wiley.
#' @keywords univar robust
#' @examples
#' 
#' huberM(c(1:9, 1000))
#' mad   (c(1:9, 1000))
#' 
#' set.seed(7)
#' x <- c(round(rnorm(1000), 1), round(rnorm(50, m=10, sd = 10)))
#' huberM(x, conf.level=0.95)
#' 
#' 
#' \dontrun{
#' 
#' # scale zero
#' huberM(rep(9, 100))
#' mad   (rep(9, 100))
#' 
#' # bootstrap confidence intervals
#' huberM(x, conf.level=0.95, method="boot")
#' }
#'  
#' 


#' @export
huberM <- function(x, conf.level = NA, sides = c("two.sided","left","right"), 
                   method = c("wald", "boot"), 
                   k = 1.345, mu = median(x), s = mad(x, center=mu),
                   na.rm = FALSE,  ...){
  
  # new interface to huberM, making it less complex
  # refer to robustbase::huberM if more control is required
  
  if(na.rm) x <- na.omit(x)
  if(anyNA(x)) return(NA)
  
  
  if(is.na(conf.level)){
    res <- .huberM(x=x, k=k, mu=mu, s=s, warn0scale=TRUE)$mu
    
    return(res)
    
  } else {
    
    switch(match.arg(method)
           ,"wald"={
             res <- .huberM(x=x, k=k, mu=mu, s=s, se=TRUE, warn0scale=TRUE)
             # Solution: (12.6.06) - Robuste Regression (Rg-2d) - Musterloeungen zu Serie 1
             # r.loc$mu + c(-1,1)*qt(0.975,8)*sqrt(t.tau/length(d.ertrag))*r.loc$s
             #
             # Ruckstuhl's Loesung:
             # (Sleep.HM$mu + c(-1,1)*qt(0.975, length(Sleep)-1) *
             #              sqrt(f.tau(Sleep, Sleep.HM$mu)) * Sleep.HM$s/sqrt(length(Sleep)))
             
             #             ci <- qnorm(1-(1-conf.level)/2) * res$SE
             ci <- qt(1-(1-conf.level)/2, length(x)-1) *
               sqrt(.tauHuber(x, res$mu, k=k)) * res$s/sqrt(length(x))
             
             res <- c(est=res$mu, lci=res$mu - ci, uci=res$mu + ci)
             
           }
           ,"boot" ={
             
             sides <- match.arg(sides, choices = c("two.sided","left","right"), several.ok = FALSE)
             if(sides!="two.sided")
               conf.level <- 1 - 2*(1-conf.level)
             
             # boot arguments in dots ...
             # adjusted bootstrap percentile (BCa) interval
             btype <- inDots(..., arg="type", default="bca")
             R <- inDots(..., arg="R", default=999)
             parallel <- inDots(..., arg="parallel", default="no")
             ncpus <- inDots(..., arg="ncpus", default=getOption("boot.ncpus", 1L))
             
             boot.fun <- boot::boot(x, 
                                    function(x, d){
                                      hm <- .huberM(x=x[d], k=k, mu=mu, s=s, se=TRUE)
                                      return(c(hm$mu, hm$s^2))
                                    }, R=R, parallel=parallel, ncpus=ncpus)
             ci <- boot::boot.ci(boot.fun, conf=conf.level, type=btype)
             
             if(btype == "norm"){
               res <- c(est=boot.fun$t0[1], lci=ci[[4]][2], uci=ci[[4]][3])
             } else {
               res <- c(est=boot.fun$t0[1], lci=ci[[4]][4], uci=ci[[4]][5])
             }
             
             if(sides=="left")        res[3] <- Inf
             else if(sides=="right")  res[2] <- -Inf
             
           }
    )
    return(res)
    
  }
  
}


# == internal helper functions ========================================

##  A modified "safe" (and more general) Huber estimator:
.huberM <-
  function(x, k = 1.345, weights = NULL,
           tol = 1e-06,
           mu = if(is.null(weights)) median(x) else .wgt.himedian(x, weights),
           s = if(is.null(weights)) mad(x, center=mu)
           else .wgt.himedian(abs(x - mu), weights),
           se = FALSE,
           warn0scale = getOption("verbose"))
  {
    ## Author: Martin Maechler, Date: 6 Jan 2003, ff
    
    ## implicit 'na.rm = TRUE':
    if(any(i <- is.na(x))) {
      x <- x[!i]
      if(!is.null(weights)) weights <- weights[!i]
    }
    n <- length(x)
    sum.w <-
      if(!is.null(weights)) {
        stopifnot(is.numeric(weights), weights >= 0, length(weights) == n)
        sum(weights)
      } else n
    it <- 0L
    NA. <- NA_real_
    if(sum.w == 0) # e.g 'x' was all NA
      return(list(mu = NA., s = NA., it = it, se = NA.)) # instead of error
    
    if(se && !is.null(weights))
      stop("Std.error computation not yet available for the case of 'weights'")
    if (s <= 0) {
      if(s < 0) stop("negative scale 's'")
      if(warn0scale && n > 1)
        warning("scale 's' is zero -- returning initial 'mu'")
    }
    else {
      wsum <- if(is.null(weights)) sum else function(u) sum(u * weights)
      repeat {
        it <- it + 1L
        y <- pmin(pmax(mu - k * s, x), mu + k * s)
        mu1 <- wsum(y) / sum.w
        if (abs(mu - mu1) < tol * s)
          break
        mu <- mu1
      }
    }
    list(mu = mu, s = s, it = it,
         SE = if(se) s * sqrt(.tauHuber(x, mu=mu, s=s, k=k) / n) else NA.)
  }




## Originally from  /u/ftp/NDK/Source-NDK-9/R/rg2-fkt.R :
.tauHuber <- function(x, mu, k=1.345, s = mad(x), resid = (x - mu)/s) {
  ## Purpose: Korrekturfaktor Tau fuer die Varianz von Huber-M-Schaetzern
  ## -------------------------------------------------------------------------
  ## Arguments: x = Daten mu = Lokations-Punkt k = Parameter der Huber Psi-Funktion
  ## -------------------------------------------------------------------------
  ## Author: Rene Locher Update: R. Frisullo 23.4.02;  M.Maechler (as.log(); s, resid)
  inr <- abs(resid) <= k
  psi  <- ifelse(inr, resid, sign(resid)*k)                # psi (x)
  psiP <- as.logical(inr)# = ifelse(abs(resid) <= k, 1, 0) # psi'(x)
  length(x) * sum(psi^2) / sum(psiP)^2
}



.wgt.himedian <- function(x, weights = rep(1,n)) {
  
  n <- length(x <- as.double(x))
  stopifnot(storage.mode(weights) %in% c("integer", "double"))
  if(n != length(weights))
    stop("'weights' must have same length as 'x'")

  if(is.integer(weights))
    wgtHimedInt_cpp(x, weights)
  else
    wgtHimed_cpp(x, weights)

}


##  A modified "safe" (and more general) Huber estimator:
.huberM <-
  function(x, k = 1.345, weights = NULL,
           tol = 1e-06,
           mu = if(is.null(weights)) median(x) else .wgt.himedian(x, weights),
           s = if(is.null(weights)) mad(x, center=mu)
           else .wgt.himedian(abs(x - mu), weights),
           se = FALSE,
           warn0scale = getOption("verbose"))
  {
    ## Author: Martin Maechler, Date: 6 Jan 2003, ff
    
    ## implicit 'na.rm = TRUE':
    if(any(i <- is.na(x))) {
      x <- x[!i]
      if(!is.null(weights)) weights <- weights[!i]
    }
    n <- length(x)
    sum.w <-
      if(!is.null(weights)) {
        stopifnot(is.numeric(weights), weights >= 0, length(weights) == n)
        sum(weights)
      } else n
    it <- 0L
    NA. <- NA_real_
    if(sum.w == 0) # e.g 'x' was all NA
      return(list(mu = NA., s = NA., it = it, se = NA.)) # instead of error
    
    if(se && !is.null(weights))
      stop("Std.error computation not yet available for the case of 'weights'")
    if (s <= 0) {
      if(s < 0) stop("negative scale 's'")
      if(warn0scale && n > 1)
        warning("scale 's' is zero -- returning initial 'mu'")
    }
    else {
      wsum <- if(is.null(weights)) sum else function(u) sum(u * weights)
      repeat {
        it <- it + 1L
        y <- pmin(pmax(mu - k * s, x), mu + k * s)
        mu1 <- wsum(y) / sum.w
        if (abs(mu - mu1) < tol * s)
          break
        mu <- mu1
      }
    }
    list(mu = mu, s = s, it = it,
         SE = if(se) s * sqrt(.tauHuber(x, mu=mu, s=s, k=k) / n) else NA.)
  }

