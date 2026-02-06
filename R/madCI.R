
#' Confidence Intervals for Median Absolute Deviations
#' 
#' A function for the median absolute deviation is included in base R,
#' \code{\link{mad}}, but there's no function for calculating confidence
#' intervals. Arachchige/Prendergast introduce interval estimators of the MAD
#' to make reliable inferences for dispersion for a single population, as well
#' as for differences and ratios of MADs for comparing two populations.
#' 
#' @name madCI
#' @aliases madCI madDiffCI madRatioCI
#' @param x a (non-empty) numeric vector of data values.
#' @param y a second (non-empty) numeric vector of data values.
#' @param conf.level confidence level of the interval.
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}. You can specify just the initial letter. \code{"left"} would
#' be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param gld.method A character string, to select the estimation method for
#' the generalized lambda distribution. One of: \code{ML} for numerical Maximum
#' Likelihood, \code{MPS} or \code{MSP} for Maximum Spacings Product, \code{TM}
#' for Titterington's Method (default), \code{SM} for Starship Method,
#' \code{TL} for method of Trimmed L-moments, \code{Lmom} for method of
#' L-moments, \code{DLA} for the method of Distributional Least Absolutes, or
#' \code{Mom} for method of Moments.  See \code{\link[gld]{fit.fkml}()}.
#' @param na.rm logical. Should missing values be removed? Defaults to
#' \code{FALSE}.
#' @param \dots further arguments, are passed on to the boot function.
#' @return a numeric vector with 3 elements: \item{mad}{median absolute
#' deviation} \item{lci}{lower bound of the confidence interval}
#' \item{uci}{upper bound of the confidence interval}
#' @author Arachchige Chandima N. P. G., Prendergast Luke A., Andri Signorell
#' <andri@@signorell.net> (only interface)
#' @seealso \code{\link{mad}}, \code{\link{madX}}
#' @references Arachchige Chandima N. P. G., Prendergast Luke A. (2019)
#' Confidence intervals for median absolute deviations, arXiv:1910.00229
#' \verb{[math.ST]}
#' @examples
#' 
#' x <- rlnorm(100)
#' y <- rlnorm(200, meanlog=1.2)
#' 
#' madCI(x)                        # single sample
#' madDiffCI(x, y)                 # two sample difference
#' madRatioCI(x, y)                # two sample squared ratio 
#' 
#' # Bootstrapinterface
#' madCI(x, method="boot", type="bca", R=499)  
#' 



#' @rdname madCI
#' @export
madCI <- function(x, 
                  conf.level = 0.95, sides = c("two.sided","left","right"), 
                  gld.method = "TM", na.rm = FALSE, ...) {
  
  if (na.rm) x <- na.omit(x)
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                     several.ok = FALSE)
  
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)
  
  
  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha/2)
  
  est <- mad.x <- mad(x)
  
  n.x <- length(x)
  asv.x <- .asv.mad(x, method = gld.method)
  
  ci <- mad.x + c(-z, z) * sqrt(asv.x / n.x)
  res <- c(est, ci)
  names(res) <- c("est","lci","uci")
  
  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- -Inf
  
  return( res )
  
}



#' @rdname madCI
#' @export
madDiffCI <- function(x, y, 
                      conf.level = 0.95, sides = c("two.sided","left","right"), 
                      gld.method = "TM", na.rm = FALSE, ...) {
  
  if (na.rm){
    x <- na.omit(x)
    y <- na.omit(y)
  } 
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                     several.ok = FALSE)
  
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)
  
  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha/2)
  
  mad.x <- mad(x)
  n.x <- length(x)
  asv.x <- .asv.mad(x, method = gld.method)
  
  mad.y <- mad(y)
  n.y <- length(y)
  asv.y <- .asv.mad(y, method = gld.method)
  
  est <- mad.x - mad.y
  ci <- est + c(-z, z) * sqrt(asv.x / n.x + asv.y / n.y)
  res <- c(est, ci)
  
  names(res) <- c("est","lci","uci")
  
  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- -Inf
  
  return( res )
  
}


#' @rdname madCI
#' @export
madRatioCI <- function(x, y,  
                       conf.level = 0.95, sides = c("two.sided","left","right"), 
                       gld.method = "TM", na.rm = FALSE, ...) {
  
  if (na.rm){
    x <- na.omit(x)
    y <- na.omit(y)
  } 
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                     several.ok = FALSE)
  
  if(sides!="two.sided")
    conf.level <- 1 - 2*(1-conf.level)
  
  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha/2)
  
  mad.x <- mad(x)
  n.x <- length(x)
  asv.x <- .asv.mad(x, method = gld.method)
  
  mad.y <- mad(y)
  n.y <- length(y)
  asv.y <- .asv.mad(y, method = gld.method)
  
  est <- (mad.x/mad.y)^2
  var.est <- 4 * est * ((1/mad.y^2) * asv.x/n.x + (est/mad.y^2) * asv.y/n.y)
  ci <- exp(log(est) + c(-z, z) * sqrt((1 / est^2) * var.est))
  
  res <- c(est, ci)
  
  names(res) <- c("est","lci","uci")
  
  if(sides=="left")
    res[3] <- Inf
  else if(sides=="right")
    res[2] <- -Inf
  
  return( res )
  
}



# internal helper functions ---------------------------------

.asv.mad <- function(x, method = "TM"){
  lambda <- gld::fit.fkml(x, method = method)$lambda
  m  <- median(x)
  mad.x <- mad(x)
  fFinv <- gld::dgl(c(m - mad.x, m + mad.x, m), lambda1 = lambda)
  FFinv <- gld::pgl(c(m - mad.x, m + mad.x), lambda1 = lambda)
  A <- fFinv[1] + fFinv[2]
  C <- fFinv[1] - fFinv[2]
  B <- C^2 + 4*C*fFinv[3]*(1 - FFinv[2] - FFinv[1])
  
  (1/(4 * A^2))*(1 + B/fFinv[3]^2)
  
} 

