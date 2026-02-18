
#' Gini Coefficient 
#' 
#' Compute the Gini coefficient, the most commonly used measure of inequality.
#' 
#' The range of the Gini coefficient goes from 0 (no concentration) to
#' \eqn{\sqrt(\frac{n-1}{n})} (maximal concentration). The bias corrected Gini
#' coefficient goes from 0 to 1.\cr The small sample variance properties of the
#' Gini coefficient are not known, and large sample approximations to the
#' variance of the coefficient are poor (Mills and Zandvakili, 1997; Glasser,
#' 1962; Dixon et al., 1987), therefore confidence intervals are calculated via
#' bootstrap re-sampling methods (Efron and Tibshirani, 1997). \cr Two types of
#' bootstrap confidence intervals are commonly used, these are percentile and
#' bias-corrected (Mills and Zandvakili, 1997; Dixon et al., 1987; Efron and
#' Tibshirani, 1997). The bias-corrected intervals are most appropriate for
#' most applications. This is set as default for the \code{type} argument
#' (\code{"bca"}). Dixon (1987) describes a refinement of the bias-corrected
#' method known as 'accelerated' - this produces values very closed to
#' conventional bias corrected intervals.\cr (Iain Buchan (2002)
#' \emph{Calculating the Gini coefficient of inequality}, see:
#' \url{https://www.statsdirect.com/help/default.htm#nonparametric_methods/gini.htm})
#' 
#' @param x a vector containing at least non-negative elements. The result will
#' be \code{NA}, if x contains negative elements. 
#' @param conf.level confidence level for the confidence interval, restricted
#' to lie between 0 and 1.  If set to \code{TRUE} the bootstrap confidence
#' intervals are calculated.  If set to \code{NA} (default) no confidence
#' intervals are returned.
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}.  You can specify just the initial letter. \code{"left"}
#' would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param method only bootstrap method is implemented.
#' @param unbiased logical. In order for G to be an unbiased estimate of the
#' true population value, calculated gini is multiplied by
#' \eqn{n/(n-1)}{n/(n-1)}. Default is TRUE. (See Dixon, 1987)
#' @param weights a numerical vector of weights the same length as \code{x}
#' giving the weights to use for elements of \code{x}.
#' @param na.rm logical, indicating whether \code{NA} values should be stripped
#' before the computation proceeds. Defaults to \code{FALSE}.
#' @param \dots the dots are passed to the function \code{\link[boot]{boot}},
#' when confidence intervals are calculated.
#' @return If \code{conf.level} is set to \code{NA} then the result will be
#' \item{a}{ single numeric value} and if a \code{conf.level} is provided, a
#' named numeric vector with 3 elements: \item{gini}{gini coefficient}
#' \item{lwr.ci}{lower bound of the confidence interval} \item{upr.ci}{upper
#' bound of the confidence interval}
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' 
#' @seealso See \code{\link{herfindahl}}, \code{\link{rosenbluth}} for
#' concentration measures, \code{\link{lc}} for the Lorenz curve\cr
#' \code{\link[ineq]{ineq}()} in the package \pkg{ineq} contains additional
#' inequality measures
#' 
#' @references Cowell, F. A. (2000) Measurement of Inequality in Atkinson, A.
#' B. / Bourguignon, F. (Eds): \emph{Handbook of Income Distribution}.
#' Amsterdam.
#' 
#' Cowell, F. A. (1995) \emph{Measuring Inequality} Harvester Wheatshef:
#' Prentice Hall.
#' 
#' Marshall, Olkin (1979) \emph{Inequalities: Theory of Majorization and Its
#' Applications}. New York: Academic Press.
#' 
#' Glasser C. (1962) Variance formulas for the mean difference and coefficient
#' of concentration. \emph{Journal of the American Statistical Association}
#' 57:648-654.
#' 
#' Mills JA, Zandvakili A. (1997). Statistical inference via bootstrapping for
#' measures of inequality. \emph{Journal of Applied Econometrics} 12:133-150.
#' 
#' Dixon, PM, Weiner J., Mitchell-Olds T, Woodley R. (1987) Boot-strapping the
#' gini coefficient of inequality. \emph{Ecology} 68:1548-1551.
#' 
#' Efron B, Tibshirani R. (1997) Improvements on cross-validation: The
#' bootstrap method. \emph{Journal of the American Statistical Association}
#' 92:548-560.
#' 
#' @family topic.Inequality
#' @concept Inequality
#' @concept Lorenz Curve
#' 
#' @examples
#' 
#' # generate vector (of incomes)
#' x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)
#' 
#' # compute gini coefficient
#' gini(x)
#' 
#' # working with weights
#' fl <- c(2.5, 7.5, 15, 35, 75, 150)    # midpoints of classes
#' n  <- c(25, 13, 10, 5, 5, 2)          # frequencies
#' 
#' # with confidence intervals
#' gini(x=fl, weights=n, conf.level=0.95, unbiased=FALSE)
#' 
#' # some special cases
#' x <- c(10, 10, 0, 0, 0)
#' # plot(lc(x))
#' 
#' gini(x, unbiased=FALSE)
#' 
#' # the same with weights
#' gini(x=c(10, 0), weights=c(2,3), unbiased=FALSE)
#' 
#' # perfect balance
#' gini(c(10, 10, 10))
#' 


#' @export
gini <- function(x, 
                 conf.level = NA, sides = c("two.sided", "left", "right"),
                 method = c("boot"), unbiased=TRUE, weights=NULL, 
                 na.rm=FALSE, ...) {
  
  # recoded for better support weights 2022-09-14
  
  # https://core.ac.uk/download/pdf/41339501.pdf
  
  if (is.null(weights)) {
    weights <- rep(1, length(x))
  }
  
  if (na.rm){
    na <- (is.na(x) | is.na(weights))
    x <- x[!na]
    weights <- weights[!na]
  } 
  
  if (any(is.na(x)) || any(x < 0)) 
    return(NA_real_)
  
  
  
  i.gini <- function(x, w, unbiased=FALSE) {
    
    w <- w/sum(w)
    
    x <- x[id <- order(x)]
    w <- w[id]
    
    f.hat <- w / 2 + c(0, head(cumsum(w), -1))
    wm <- meanX(x, w)
    
    res <- 2 / wm * sum(w * (x - wm) * (f.hat - meanX(f.hat, w)))
    
    if(unbiased)
      res <- res * 1/(1 - sum(w^2))
    
    return(res)
  }
  
  
  if (is.na(conf.level)) {
    res <- i.gini(x, weights, unbiased = unbiased)
    
  } else {
    
    sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                       several.ok = FALSE)
    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)
    
    
    # boot.gini <- boot(data = x,
    #                   statistic = function(z, i, u, unbiased) 
    #                     i.gini(x = z[i], w = u[i], unbiased = unbiased), 
    #                   R=R, u=weights, unbiased=unbiased)
    # ci <- boot.ci(boot.gini, conf = conf.level, type = type)
    # res <- c(gini = boot.gini$t0, lwr.ci = ci[[4]][4], upr.ci = ci[[4]][5])
    
    
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
                           function(z, i, u, unbiased) 
                             i.gini(x = z[i], w = u[i], unbiased = unbiased), 
                           u=weights, unbiased=unbiased, 
                           R=R, parallel=parallel, ncpus=ncpus)
    ci <- boot::boot.ci(boot.fun, conf=conf.level, type=btype)
    
    if(btype == "norm"){
      res <- c(est=boot.fun$t0, lci=ci[[4]][2], uci=ci[[4]][3])
    } else {
      res <- c(est=boot.fun$t0, lci=ci[[4]][4], uci=ci[[4]][5])
    }
    
    
    
  }
  
  return(res)
  
}



# == some history ==========================================================

# Original Zeileis:
# gini <- function(x)
# {
#   n <- length(x)
#   x <- sort(x)
#   G <- sum(x * 1:n)
#   G <- 2*G/(n*sum(x))
#   G - 1 - (1/n)
# }


# other:
# http://rss.acs.unt.edu/Rdoc/library/reldist/html/gini.html
# http://finzi.psych.upenn.edu/R/library/dplR/html/gini.coef.html



# gini <- function(x, n = rep(1, length(x)), unbiased = TRUE, conf.level = NA, R = 1000, type = "bca", na.rm = FALSE) {
# 
#   # cast to numeric, as else sum(x * 1:n) might overflow for integers
#   # http://stackoverflow.com/questions/39579029/integer-overflow-error-using-gini-function-of-package-desctools
#   x <- as.numeric(x)
# 
#   x <- rep(x, n)    # same handling as Lc
#   if(na.rm) x <- na.omit(x)
#   if (any(is.na(x)) || any(x < 0)) return(NA_real_)
# 
#   i.gini <- function (x, unbiased = TRUE){
#     n <- length(x)
#     x <- sort(x)
# 
#     res <- 2 * sum(x * 1:n) / (n*sum(x)) - 1 - (1/n)
#     if(unbiased) res <- n / (n - 1) * res
# 
#     # limit gini to 0 here, if negative values appear, which is the case with
#     # gini( c(10,10,10))
#     return( pmax(0, res))
# 
#     # other guy out there:
#     #     N <- if (unbiased) n * (n - 1) else n * n
#     #     dsum <- drop(crossprod(2 * 1:n - n - 1, x))
#     #     dsum / (mean(x) * N)
#     # is this slower, than above implementation??
#   }
# 
#   if(is.na(conf.level)){
#     res <- i.gini(x, unbiased = unbiased)
# 
#   } else {
#     # adjusted bootstrap percentile (BCa) interval
#     boot.gini <- boot(x, function(x, d) i.gini(x[d], unbiased = unbiased), R=R)
#     ci <- boot.ci(boot.gini, conf=conf.level, type=type)
#     res <- c(gini=boot.gini$t0, lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
#   }
# 
#   return(res)
# 
# }


