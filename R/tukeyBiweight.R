

#' Calculate Tukey's Biweight Robust Mean
#' 
#' This calculates a robust average that is unaffected by outliers.
#' 
#' This is a one step computation that follows the Affy whitepaper below, see
#' page 22. \code{\var{const}} determines the point at which outliers are given
#' a weight of 0 and therefore do not contribute to the calculation of the
#' mean.  \code{\var{const} = 9} sets values roughly +/-6 standard deviations
#' to 0. \code{\var{const} = 6} is also used in tree-ring chronology
#' development. Cook and Kairiukstis (1990) have further details.
#' 
#' An exact summation algorithm (Shewchuk 1997) is used. When some assumptions
#' about the rounding of floating point numbers and conservative compiler
#' optimizations hold, summation error is completely avoided.  Whether the
#' assumptions hold depends on the platform, i.e. compiler and \acronym{CPU}.
#' 
#' @param x a \code{numeric} vector
#' @param conf.level confidence level of the interval. If set to \code{NA}
#' (which is the default) no confidence interval will be calculated.
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}.  You can specify just the initial letter. \code{"left"}
#' would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param method only bootstrap method is implemented.
#' @param const a constant. \code{\var{const}} is preassigned a value of 9
#' according to the Cook reference below but other values are possible.
#' @param na.rm logical, indicating whether \code{NA} values should be stripped
#' before the computation proceeds. Defaults to FALSE.
#' @param \dots the dots are passed to the function \code{\link[boot]{boot}},
#' when confidence intervals are calculated.
#' @return A \code{numeric} mean.
#' @author originally written by Mikko Korpela <mikko.korpela@@aalto.fi>\cr
#' Rcpp port by Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{huberM}}, \code{\link{rangeX}}, \code{\link{scaleX}}
#' @references
#' 
#' Statistical Algorithms Description Document, 2002, Affymetrix.
#' 
#' Cook, E. R. and Kairiukstis, L. A. (1990) \emph{Methods of Dendrochronology:
#' Applications in the Environmental Sciences}.  Springer.  \acronym{ISBN-13}:
#' 978-0792305866.
#' 
#' Mosteller, F. and Tukey, J. W. (1977) \emph{Data Analysis and Regression: a
#' second course in statistics}.  Addison-Wesley.  \acronym{ISBN-13}:
#' 978-0201048544.
#' 
#' Shewchuk, J. R. (1997) Adaptive Precision Floating-Point Arithmetic and Fast
#' Robust Geometric Predicates.  \emph{Discrete and Computational Geometry},
#' 18(3):305-363. Springer.
#' @keywords robust univar
#' @examples
#' 
#' tukeyBiweight(rnorm(100))


#' @export
tukeyBiweight <- function(x, conf.level = NA, sides = c("two.sided","left","right"), 
                          method = c("boot"), const=9,  na.rm = FALSE,  ...) {
  
  if(na.rm) x <- na.omit(x)
  if(anyNA(x)) return(NA)
  
  if(is.na(conf.level)){
    res <- tbrm(x, const)
    
  } else {
    
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
                           function(x, d) 
                             tbrm(x[d], const), 
                           R=R, parallel=parallel, ncpus=ncpus)
    ci <- boot::boot.ci(boot.fun, conf=conf.level, type=btype)
    
    if(btype == "norm"){
      res <- c(est=boot.fun$t0, lci=ci[[4]][2], uci=ci[[4]][3])
    } else {
      res <- c(est=boot.fun$t0, lci=ci[[4]][4], uci=ci[[4]][5])
    }
    
    if(sides=="left")        res[3] <- Inf
    else if(sides=="right")  res[2] <- -Inf
    
  }
  
  return(res)
  
}

