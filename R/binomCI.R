

#' Confidence Intervals for Binomial Proportions
#' 
#' \code{binomCI()} computes confidence intervals for binomial proportions
#' using a wide range of commonly proposed methods.
#'
#' All arguments are vectorized and recycled according to standard R rules.
#'
#' \strong{Wald}:
#' Obtained by inverting the acceptance region of the large-sample normal
#' (Wald) test.
#'
#' \strong{Wald with continuity correction}:
#' A continuity-corrected version of the Wald interval, obtained by adding
#' 1/(2n) to the standard Wald limits.
#'
#' \strong{Wilson} (default):
#' Introduced by Wilson (1927), this interval is obtained by inverting the
#' central limit theorem approximation to the family of equal-tail tests of
#' \eqn{p = p_0}. It is recommended by Agresti and Coull (1998) and
#' Brown et al. (2001). The same interval is returned as \code{conf.int}
#' by \code{\link{prop.test}} with \code{correct = FALSE}.
#'
#' \strong{Wilson with continuity correction}:
#' A continuity-corrected modification of the Wilson interval. This
#' corresponds to \code{\link{prop.test}} with \code{correct = TRUE}.
#'
#' \strong{Modified Wilson}:
#' An adjustment of the Wilson interval for extreme counts
#' (i.e., \eqn{x} close to 0 or \eqn{n}), as proposed by Brown et al. (2001).
#'
#' \strong{Agresti-Coull}:
#' A simplified modification of the Wilson interval (Agresti and Coull, 1998).
#' These intervals are never shorter than the Wilson intervals
#' (Brown et al., 2001). The internally used adjusted estimator
#' \eqn{\tilde{p}} is returned as an attribute.
#'
#' \strong{Jeffreys}:
#' The equal-tailed Bayesian interval based on the Jeffreys prior,
#' as described in Brown et al. (2001).
#'
#' \strong{Modified Jeffreys}:
#' A modification of the Jeffreys interval for boundary cases
#' (e.g., \eqn{x = 0}, \eqn{x = n}, or near-boundary values),
#' following Brown et al. (2001).
#'
#' \strong{Clopper-Pearson}:
#' The so-called exact interval, based on quantiles of the
#' corresponding beta distribution.
#'
#' \strong{Arcsine}:
#' Based on the variance-stabilizing arcsine transformation for
#' the binomial distribution.
#'
#' \strong{Logit}:
#' Obtained by constructing a Wald-type interval on the log-odds scale
#' and transforming back to the probability scale.
#'
#' \strong{Witting}:
#' A randomized procedure (Witting, 1985) providing uniformly optimal
#' lower and upper confidence bounds for binomial proportions.
#' Repeated calls may yield slightly different results unless the
#' random number generator seed is fixed.
#'
#' \strong{Pratt}:
#' Based on a highly accurate normal approximation (Pratt, 1968).
#'
#' \strong{Mid-p}:
#' Designed to reduce the conservatism of the Clopper-Pearson interval.
#' The lower bound \eqn{p_l} solves
#' \deqn{\frac{1}{2} f(x; n, p_l) + (1 - F(x; n, p_l)) = \frac{\alpha}{2}}
#' and the upper bound \eqn{p_u} solves
#' \deqn{\frac{1}{2} f(x; n, p_u) + F(x - 1; n, p_u) = \frac{\alpha}{2}}
#' where \eqn{f} and \eqn{F} denote the binomial probability mass and
#' cumulative distribution functions. For \eqn{x = 0} the lower bound
#' is set to 0; for \eqn{x = n} the upper bound is set to 1.
#'
#' \strong{Likelihood-based}:
#' Confidence intervals obtained by profiling the binomial deviance
#' in the neighbourhood of the maximum likelihood estimator.
#'
#' \strong{Blaker}:
#' An exact interval based on the method proposed by Blaker (2000).
#'
#' Some methods may produce limits outside the admissible range
#' \eqn{[0, 1]}. In such cases, the bounds are truncated to remain
#' within the valid parameter space.
#'
#' \strong{Which interval should be used?}
#' The Wald interval is known to have poor coverage properties,
#' particularly for small sample sizes or proportions near 0 or 1.
#' In contrast, the Clopper-Pearson interval is conservative and
#' often unnecessarily wide. Brown et al. (2001) recommend the
#' Wilson or Jeffreys intervals for small sample sizes, and
#' the Agresti-Coull, Wilson, or Jeffreys intervals for larger samples,
#' as providing more reliable coverage than most alternatives.
#'
#' For the methods \code{"wilson"}, \code{"wilson-cc"},
#' \code{"wilson-mod"}, \code{"agresti-coull"},
#' \code{"witting"}, and \code{"arcsine"},
#' the internally used adjusted point estimator can be returned
#' by setting \code{std_est = FALSE}. These estimators are typically
#' slightly shrunk toward 0.5 compared to the usual estimator \eqn{x/n}.
#' See the cited literature for further details.
#' 
#' @param x number of successes.
#' @param n number of trials.
#' @param conf.level confidence level, defaults to 0.95.
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}. You can specify just the initial letter. \code{"left"}
#' would be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param method character string specifying which method to use; this can be
#' one out of: \code{"wald"}, \code{"wald-cc"},\code{"wilson"} (default), 
#' \code{"wilson-cc"},
#' \code{"agresti-coull"}, \code{"jeffreys"}, \code{"wilson-mod"},
#' \code{"jeffreys-mod"}, \code{"clopper-pearson"}, \code{"arcsine"},
#' \code{"logit"}, \code{"witting"}, \code{"pratt"}, \code{"mid-p"},
#' \code{"likelihood"} and \code{"blaker"}.  All the methods can be
#' asked by \code{".all"}. Abbreviation of method is 
#' accepted. See details.
#' 
#' @param std_est logical, specifying if the standard point estimator for the
#' proportion value \code{x/n} should be returned (\code{TRUE}, default) or
#' the method-specific internally used alternative point estimate
#' (\code{FALSE}).
#' 
#' @return A named vector with 3 elements \code{est, lci, uci}
#' for estimate, lower and upper confidence interval.
#' 
#' For more than one argument each, a 3-column matrix is returned.
#' 
#' @section Contributors:
#' The function is based on earlier work by Matthias Kohl in package \pkg{SLmisc},
#' whose original implementation provided the methodological foundation
#' The implementations of the Pratt, Mid-p, and Blaker methods are
#' based on contributions by Rand R. Wilcox, Michael Hoehle,
#' and Ralph Scherer, respectively.
#' 
#' The current implementation was written and is maintained by
#' Andri Signorell. 
#' 
#'    
#' @references Agresti A. and Coull B.A. (1998) Approximate is better than
#' "exact" for interval estimation of binomial proportions.  \emph{American
#' Statistician}, \bold{52}, pp. 119-126.
#' 
#' Brown L.D., Cai T.T. and Dasgupta A. (2001) Interval estimation for a
#' binomial proportion \emph{Statistical Science}, \bold{16}(2), pp. 101-133.
#' 
#' Witting H. (1985) \emph{Mathematische Statistik I}. Stuttgart: Teubner.
#' 
#' Pratt J. W. (1968) A normal approximation for binomial, F, Beta, and other
#' common, related tail probabilities \emph{Journal of the American
#' Statistical Association}, 63, 1457- 1483.
#' 
#' Wilcox, R. R. (2005) \emph{Introduction to robust estimation and hypothesis
#' testing}. Elsevier Academic Press
#' 
#' Newcombe, R. G. (1998) Two-sided confidence intervals for the single
#' proportion: comparison of seven methods, \emph{Statistics in Medicine},
#' 17:857-872 https://pubmed.ncbi.nlm.nih.gov/16206245/
#' 
#' Blaker, H. (2000) Confidence curves and improved exact confidence intervals
#' for discrete distributions, \emph{Canadian Journal of Statistics} 28 (4),
#' 783-798
#' 
#' @seealso \code{\link[stats]{binom.test}}, \code{\link[Hmisc]{binconf}}
#'  
#' @family topic.categoricalData
#' @concept categorical data
#' @concept confidence intervals
#'  
#' @examples
#' 
#' binomCI(x=37, n=43, 
#'         method=eval(formals(binomCI)$method))   # return all methods
#' 
#' prop.test(x=37, n=43, correct=FALSE) # same as method wilson
#' prop.test(x=37, n=43, correct=TRUE)  # same as method wilsoncc
#' 
#' 
#' # the confidence interval computed by binom.test
#' #   corresponds to the Clopper-Pearson interval
#' binomCI(x=42, n=43, method="clopper-pearson")
#' binom.test(x=42, n=43)$conf.int
#' 
#' 
#' # all arguments are being recycled:
#' binomCI(x=c(42, 35, 23, 22), n=43, method="wilson")
#' binomCI(x=c(42, 35, 23, 22), n=c(50, 60, 70, 80), method="jeffreys")
#' 
#' # example Table I in Newcombe (1998)
#' meths <- c("wald", "wald-cc", "wilson", "wilson-cc",
#'            "clopper-pearson","mid-p", "lik")
#' setNamesX(cbind(round(cbind(
#'     binomCI(81, 263, m=meths)[, c("lci","uci")],
#'     binomCI(15, 148, m=meths)[,  c("lci","uci")],
#'     binomCI(0, 20, m=meths)[, c("lci","uci")],
#'     binomCI(1, 29, m=meths)[, c("lci","uci")]), 4)), 
#'   rownames=meths)
#' 
#' # returning p.tilde for agresti-coull ci
#' binomCI(x=81, n=263, meth="agresti-coull", std_est = c(TRUE, FALSE))
#' 
#' # return all implemented methods
#' binomCI(4, 19, conf.level =0.95, 
#'         method = c(".all"))[, c("est","lci","uci","method")]
#' 



#' @rdname binomCI
#' @export
binomCI <- function(x, n, 
                    conf.level = 0.95, 
                    sides = c("two.sided","left","right"),
                    method = c("wilson", "wilson-cc", "wilson-mod",
                               "wald", "wald-cc",
                               "jeffreys", "jeffreys-mod",
                               "clopper-pearson", "agresti-coull",
                               "pratt", "arcsine", "logit",
                               "witting", "mid-p","blaker",
                               "likelihood" ), 
                    std_est=TRUE) {
  
  
  sides <- match.arg(sides)
  
  if (missing(method)) {
    # if not provided take the first method instead of all (!)
    method <- eval(formals(sys.function())$method)[1]
    
  } else {
    # resolve methods cleanly, allowing an ".all" hidden option for method
    method <- .resolveMethod(method, several.ok = TRUE)
  }
  
  res <- .recycleApply(.binomCI_engine,
                       x = x,
                       n = n,
                       conf.level = conf.level,
                       sides = sides,
                       method = method,
                       std_est = std_est
                       )
  
  if(length(res) == 1)
    out <- res[[1]]
  else{
    out <- as.data.frame(attr(res, "recycle"))
    out <- data.frame(do.call(rbind, res), out)
  }

  return(out)
  
}



.binomCI_engine <- function(x, n, conf.level, sides, method, std_est){
  
  alpha <- 1 - conf.level
  if (sides != "two.sided")
    alpha <- alpha / 2

  CI <- switch( method
                , "wald" =              { .binomCI.wald(x, n, alpha) }
                , "wald-cc" =           { .binomCI.wald(x, n, alpha, corr=TRUE) }
                , "jeffreys" =          { .binomCI.jeffreys(x, n, alpha) }
                , "jeffreys-mod" =      { .binomCI.jeffreys_mod(x, n, alpha) }
                , "clopper-pearson" =   { .binomCI.clopper_pearson(x, n, alpha) }
                , "arcsine" =           { .binomCI.arcsine(x, n, alpha) }
                , "logit" =             { .binomCI.logit(x, n, alpha) }
                , "witting" =           { .binomCI.witting(x, n, alpha) }
                , "agresti-coull" =     { .binomCI.agresti_coull(x, n, alpha) }
                , "pratt" =             { .binomCI.pratt(x, n, alpha) }
                , "wilson" =            { .binomCI.wilson(x, n, alpha) }
                , "wilson-cc" =         { .binomCI.wilson_cc(x, n, alpha) }
                , "wilson-mod" =        { .binomCI.wilson_mod(x, n, alpha) }
                , "mid-p" =             { .binomCI.midp(x, n, alpha) }
                , "blaker" =            { .binomCI.blaker(x, n, alpha) }
                , "likelihood" =        { .binomCI.lik(x, n, alpha) }
                , stop(gettextf("Unknown method '%s'.", method))
  )
  
  # this is the default estimator used by the most (but not all) methods
  est <- x/n
  
  if(!std_est){
    
    if(method %in% 
       c("agresti-coull", "wilson", "wilson-cc", "wilson-mod"))
      est <- .binomCI.nonStdEst(x, n, alpha)
    
    else if(method %in% c("arcsine", "witting"))
      est <- attr(CI, "p.tilde")
  }
  
  # dot not return ci bounds outside [0,1]
  ci <- c( est = est, 
           lci = max(0, CI["lci"]), 
           uci = min(1, CI["uci"]) )
  
  if(sides=="left")
    ci[3] <- 1
  else if(sides=="right")
    ci[2] <- 0
  
  return(ci)

}



# ==  internal helper functions  ===========================================

#' @keywords internal
.binomCI.wilson <- function(x, n, alpha) {
  
  p.hat <- x/n
  q.hat <- 1 - p.hat
  z <- qnorm(1-alpha/2)
  z2 <- z^2
  
  term1 <- (x + z2/2) / (n + z2)
  term2 <- z * sqrt(n) / (n + z2) * 
    sqrt(p.hat * q.hat + z2 / (4 * n))
  
  return(c(
    lci = max(0, term1 - term2),
    uci = min(1, term1 + term2)
  ))
  
}


#' @keywords internal
.binomCI.wilson_cc <- function(x, n, alpha) {
  
  p.hat <- x/n
  q.hat <- 1 - p.hat
  z <- qnorm(1 - alpha/2)
  z2 <- z^2
  
  lci <- ( 2 * x + z2 - 1 - z * sqrt(z2 - 2 - 1/n + 
                                       4 * p.hat * (n * q.hat + 1))) / (2 * (n + z2))
  
  uci <- ( 2 * x + z2 + 1 + z * sqrt(z^2 + 2 - 1/n + 
                                       4 * p.hat * (n * q.hat - 1))) / (2 * (n + z2))
  
  return( c(
    lci = max(0, ifelse(p.hat == 0, 0, lci)),
    uci = min(1, ifelse(p.hat == 1, 1, uci)))
  )
  
}


#' @keywords internal
.binomCI.wilson_mod <- function(x, n, alpha) {
  
  p.hat <- x/n
  q.hat <- 1 - p.hat
  z <- qnorm(1 - alpha/2)
  z2 <- z^2
  
  term1 <- (x + z2/2) / (n + z2)
  term2 <- z * sqrt(n) / (n + z2) * sqrt(p.hat * q.hat + z2 / (4 * n))
  
  if((n <= 50 & x %in% c(1, 2)) | (n >= 51 & x %in% c(1:3)))
    lci <- 0.5 * qchisq(alpha, 2 * x)/n
  else
    lci <-  max(0, term1 - term2)
  
  if((n <= 50 & x %in% c(n-1, n-2)) | (n >= 51 & x %in% c(n-(1:3))))
    uci <- 1 - 0.5 * qchisq(alpha, 2 * (n - x))/n
  else
    uci <- min(1, term1 + term2)
  
  return( c( lci = lci, uci = uci) )
  
}



#' @keywords internal
.binomCI.agresti_coull <- function(x, n, alpha)  {
  
  z <- qnorm(1-alpha/2)
  
  n.tilde <- n + z^2
  p.tilde <- .binomCI.nonStdEst(x, n, z)
  q.tilde <- 1 - p.tilde
  
  term2 <- z * sqrt(p.tilde * q.tilde) / sqrt(n.tilde)
  
  return( c(
    lci = max(0, p.tilde - term2),
    uci = min(1, p.tilde + term2))
  )
  
}



#' @keywords internal
.binomCI.wald <- function(x, n, alpha, corr=FALSE){
  
  p.hat <- x/n
  
  # margin of error
  ME <- qnorm(1 - alpha/2) * sqrt(p.hat * (1 - p.hat) / n)
  
  # continuity correction
  if(corr)
    ME <- ME + 1/(2*n)
  
  lci <- max(0, p.hat - ME)
  uci <- min(1, p.hat + ME)
  
  return(c(lci=lci, uci=uci))
}


#' @keywords internal
.binomCI.jeffreys <- function(x, n, alpha){
  
  return(c( 
    lci = if(x == 0) 0 
    else qbeta(alpha/2, x + 0.5, n - x + 0.5),
    uci = if(x == n) 1 
    else qbeta(1-alpha/2, x + 0.5, n - x + 0.5)
  ))
  
}


#' @keywords internal
.binomCI.jeffreys_mod <- function(x, n, alpha)  {
  
  return(c(
    lci =
      if (x == n) { 
        (alpha/2)^(1/n) 
      } else if (x <= 1) {
        0
      } else {
        qbeta(alpha/2, x + 0.5, n - x + 0.5)
      },
    
    uci =
      if (x == 0) {
        1 - (alpha/2)^(1/n)
      } else if (x >= n - 1) {
        1
      } else {
        qbeta(1 - alpha/2, x + 0.5, n - x + 0.5)
      }
  ))
  
}


#' @keywords internal
.binomCI.clopper_pearson <- function(x, n, alpha){
  return(c(
    lci = if (x == 0) 0 else qbeta(alpha/2, x, n - x + 1),
    uci = if (x == n) 1 else qbeta(1 - alpha/2, x + 1, n - x)
  ))
}


#' @keywords internal
.binomCI.arcsine <- function(x, n, alpha) {
  
  p.tilde <- (x + 0.375)/(n + 0.75)
  ME <- 0.5 * qnorm(1-alpha/2) / sqrt(n)
  
  res <- c(
    lci = sin(asin(sqrt(p.tilde)) - ME)^2,
    uci = sin(asin(sqrt(p.tilde)) + ME)^2
  ) 
  attr(res, "p.tilde") <- p.tilde
  
  return(res)  
  
}


#' @keywords internal
.binomCI.logit <- function(x, n, alpha){
  
  setNamesX(
    
    logitInv(log(x/(n-x)) - 
               c(1,-1) * qnorm(1-alpha/2) * sqrt(n/(x*(n-x)))),
    
    names=c("lci", "uci"))
}


#' @keywords internal
.binomCI.witting <- function(x, n, alpha) {
  
  # here the uniform random number is by design
  # define set.seed() before calling the function, if you want
  # reproducible results
  x.tilde <- x + runif(1, min = 0, max = 1)
  
  pbinom.abscont <- function(q, size, prob){
    v <- trunc(q)
    return( pbinom(v-1, size = size, prob = prob) +
              (q - v) * dbinom(v, size = size, prob = prob))
  }
  
  qbinom.abscont <- function(p, size, x){
    
    fun <- function(prob, size, x, p){
      pbinom.abscont(x, size, prob) - p
    }
    uniroot(fun, interval = c(0, 1), size = size, x = x, p = p)$root
  }
  
  res <- c(
    lci = qbinom.abscont(1-alpha, size = n, x = x.tilde),
    uci = qbinom.abscont(alpha, size = n, x = x.tilde)
  )
  attr(res, "p.tilde") <- x.tilde / n
  
  return(res)
  
}


#' @keywords internal
.binomCI.pratt <- function(x, n, alpha) {
  
  if(x==0) {
    lci <- 0
    uci <- 1 - alpha^(1/n)
  } else if(x==1) {
    lci <- 1 - (1 - alpha/2)^(1/n)
    uci <- 1 - (alpha/2)^(1/n)
  } else if(x==(n-1)) {
    lci <- (alpha/2)^(1/n)
    uci <- (1 - alpha/2)^(1/n)
  } else if(x==n) {
    lci <- alpha^(1/n)
    uci <- 1
    
  } else {
    z <- qnorm(1 - alpha/2)
    
    A <- ((x+1) / (n-x))^2
    B <- 81*(x+1)*(n-x)-9*n-8
    C <- (0-3)*z*sqrt(9*(x+1)*(n-x)*(9*n+5-z^2)+n+1)
    D <- 81*(x+1)^2-9*(x+1)*(2+z^2)+1
    E <- 1+A*((B+C)/D)^3
    uci <- 1/E
    
    A <- (x / (n-x-1))^2
    B <- 81*x*(n-x-1)-9*n-8
    C <- 3*z*sqrt(9*x*(n-x-1)*(9*n+5-z^2)+n+1)
    D <- 81*x^2-9*x*(2+z^2)+1
    E <- 1+A*((B+C)/D)^3
    lci <- 1/E
  }
  
  return(c(lci = lci, uci = uci))
  
}


#' @keywords internal
.binomCI.midp <- function(x, n, alpha){
  
  # Functions to find root of for the lower and higher bounds of the CI
  low <- function(x, n, p) {
    0.5 * dbinom(x, size=n, prob=p) + 
      pbinom(x, size=n, prob=p, lower.tail=FALSE) - alpha/2
  }
  
  upr <- function(x, n, p) {
    0.5 * dbinom(x, size=n, prob=p) + 
      pbinom(x - 1, size=n, prob=p) - alpha/2
  }
  
  # pick lci = 0 when x = 0 and uci = 1 when x = n
  lci <- 0
  uci <- 1
  p.hat <- x/n
  
  # Calculate CI by finding roots of the funcs
  if (x!=0) {
    lci <- uniroot(low, interval=c(0, p.hat), x=x, n=n)$root
  } 
  if (x!=n) {
    uci  <- uniroot(upr, interval=c(p.hat, 1), x=x, n=n)$root
  }
  
  return(c(lci = lci, uci = uci))
  
}



#' @keywords internal
.binomCI.blaker <- function(x, n, alpha) {
  
  # use fast Rcpp version of acceptBin ( ~ 2x as fast as R-version)
  # acceptbin <- function(p) {
  #   p1 <- 1 - pbinom(x - 1, n, p)
  #   p2 <- pbinom(x, n, p)
  #   a1 <- p1 + pbinom(qbinom(p1, n, p) - 1, n, p)
  #   a2 <- p2 + 1 - pbinom(qbinom(1 - p2, n, p), n, p)
  #   min(a1, a2)
  # }
  
  
  ## -------- lower CI --------
  find_lci <- function(lo) {
    if (acceptBin(x, n, lo) >= alpha) return(lo)
    left <- lo; right <- 1
    for (i in 1:60) {
      mid <- (left + right)/2
      if (acceptBin(x, n, mid) >= alpha) right <- mid else left <- mid
    }
    right
  }
  
  ## -------- upper CI --------
  find_uci <- function(up) {
    
    # 1) first go left, until we are >= alpha
    p <- up
    step <- (up - 0) / 1000
    
    while (p > 0 && acceptBin(x, n, p) < alpha)
      p <- p - step
    
    if (p <= 0)
      stop("No valid upper CI found (should not happen)")
    
    # 2) now we have:
    #    acceptbin(p) >= alpha
    #    acceptbin(p + step) < alpha
    left <- p
    right <- min(p + step, up)
    
    # 3) bisection
    for (i in 1:60) {
      mid <- (left + right)/2
      if (acceptBin(x, n, mid) >= alpha)
        left <- mid
      else
        right <- mid
    }
    left
  }
  
  lci <- 0
  uci <- 1
  
  if (x != 0) {
    lo <- qbeta(alpha/2, x, n - x + 1)
    lci <- find_lci(lo)
  }
  
  if (x != n) {
    up <- qbeta(1 - alpha/2, x + 1, n - x)
    uci <- find_uci(up)
  }
  
  c(lci = lci, uci = uci)
}



#' @keywords internal
.binomCI.lik <- function(x, n, alpha) {
  
  p.hat <- x/n
  lci <- 0
  uci <- 1
  z <- qnorm(1 - alpha * 0.5)
  
  # preset tolerance, should we offer function argument?
  tol <- .Machine$double.eps^0.5
  
  BinDev <- function(y, x, mu, wt, bound = 0, 
                     tol = .Machine$double.eps^0.5, ...) {
    
    # returns the binomial deviance for y, x, wt
    ll.y <- ifelse(y %in% c(0, 1), 0, dbinom(x, wt, y, log=TRUE))
    ll.mu <- ifelse(mu %in% c(0, 1), 0, dbinom(x, wt, mu, log=TRUE))
    res <- ifelse(abs(y - mu) < tol, 0, 
                  sign(y - mu) * sqrt(-2 * (ll.y - ll.mu)))
    return(res - bound)
  }
  
  if(x != 0 && tol < p.hat) {
    lci <- if(BinDev(tol, x, p.hat, n, -z, tol) <= 0) {
      uniroot(f = BinDev, 
              interval = c(tol, if(p.hat < tol || p.hat == 1) 1 - tol else p.hat), 
              bound = -z, x = x, mu = p.hat, wt = n)$root }
  }
  
  if(x != n && p.hat < (1-tol)) {
    uci <- if(BinDev(y = 1 - tol, x = x, mu = ifelse(p.hat > 1 - tol, tol, p.hat), 
                     wt = n, bound = z, tol = tol) < 0) {
      
      lci <- if(BinDev(tol, x, if(p.hat < tol || p.hat == 1) 1 - tol else p.hat, n, -z, tol) <= 0) {
        uniroot(f = BinDev, interval = c(tol, p.hat),
                bound = -z, x = x, mu = p.hat, wt = n)$root  }
      
    } else {
      
      uniroot(f = BinDev, interval = c(if(p.hat > 1 - tol) tol else p.hat, 1 - tol),
              bound = z, x = x, mu = p.hat, wt = n)$root     }
  }
  
  return(c(lci = lci, uci = uci))
  
}



#' @keywords internal
.binomCI.nonStdEst <- function(x, n, alpha){
  z2 <- qnorm(1-alpha/2)^2
  # p.tilde
  return( (x + z2/2) / (n + z2)) 
}


