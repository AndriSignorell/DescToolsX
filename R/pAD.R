

#' Null Distribution of Anderson-Darling Test Statistic
#' 
#' \code{pAD} computes the cumulative distribution function, and \code{qAD}
#' computes the quantile function, of the null distribution of the
#' Anderson-Darling test statistic.
#' 
#' \code{pAD} uses the algorithms and C code described in Marsaglia and
#' Marsaglia (2004).
#' 
#' \code{qAD} uses \code{\link[stats]{uniroot}} to find the quantiles.
#' 
#' The argument \code{fast} applies only when \code{n=Inf} and determines
#' whether the asymptotic distribution is approximated using the faster
#' algorithm \code{adinf} (accurate to 4-5 places) or the slower algorithm
#' \code{ADinf} (accurate to 11 places) described in Marsaglia and Marsaglia
#' (2004).
#' 
#' @name pAD
#' @aliases pAD qAD
#' @param q Numeric vector of quantiles (values for which the cumulative
#' probability is required).
#' @param p Numeric vector of probabilities.
#' @param n Integer. Sample size for the Anderson-Darling test.
#' @param lower.tail Logical. If \code{TRUE} (the default), probabilities are
#' \eqn{P(X \le q)}{P(X <= q)}, and otherwise they are \eqn{P(X > q)}.
#' @param fast Logical value indicating whether to use a fast algorithm or a
#' slower, more accurate algorithm, in the case \code{n=Inf}.
#' @return A numeric vector of the same length as \code{p} or \code{q}.
#' @author Original C code by G. and J. Marsaglia.  \R interface by Adrian
#' Baddeley.
#' @seealso \code{\link{andersonDarlingTest}}
#' @references Anderson, T.W. and Darling, D.A. (1952) Asymptotic theory of
#' certain 'goodness-of-fit' criteria based on stochastic processes.
#' \emph{Annals of Mathematical Statistics} \bold{23}, 193--212.
#' 
#' Anderson, T.W. and Darling, D.A. (1954) A test of goodness of fit.
#' \emph{Journal of the American Statistical Association} \bold{49}, 765--769.
#' 
#' Marsaglia, G. and Marsaglia, J. (2004) Evaluating the Anderson-Darling
#' Distribution.  \emph{Journal of Statistical Software} \bold{9} (2), 1--5.
#' February 2004.  \url{http://www.jstatsoft.org/v09/i02}
#' @keywords distribution htest
#' @examples
#' 
#'   pAD(1.1, n=5)
#'   pAD(1.1)
#'   pAD(1.1, fast=FALSE)
#' 
#'   qAD(0.5, n=5)
#'   qAD(0.5)
#' 
#' 


#' @rdname pAD
#' @export
pAD <- function(q, n=Inf, lower.tail=TRUE, fast=TRUE) {
  
  q <- as.numeric(q)
  p <- rep(NA_real_, length(q))
  if(any(ones <- is.infinite(q) & (q == Inf)))
    p[ones] <- 1
  if(any(zeroes <- (is.finite(q) & q <= 0) | (is.infinite(q) & (q == -Inf))))
    p[zeroes] <- 0
  ok <- is.finite(q) & (q > 0)
  nok <- sum(ok)
  
  if(nok > 0) {
    if(is.finite(n)) {
      p[ok] <- ADprobN(q[ok], n)
    } else if(fast) {
      ## fast version adinf()
      p[ok] <- ADprobApproxInf(q[ok])
    } else {
      ## slow, accurate version ADinf()
      p[ok] <- ADprobExactInf(q[ok])
    }
  }
  
  if(!lower.tail)
    p <- 1 - p
  
  return(p)
}


#' @rdname pAD
#' @export
qAD <- local({
  
  f <- function(x, N, P, Fast) {
    pAD(x, N, fast=Fast) - P
  }
  
  qAD <- function(p, n=Inf, lower.tail=TRUE, fast=TRUE) {
    ## quantiles of null distribution of Anderson-Darling test statistic
    stopifnot(all(p >= 0))
    stopifnot(all(p <= 1))
    if(!lower.tail) p <- 1-p
    ans <- rep(NA_real_, length(p))
    for(i in which(p >= 0 & p < 1)) 
      ans[i] <- uniroot(f, c(0, 1), N=n, P=p[i], Fast=fast, extendInt="up")$root
    return(ans)
  }
  
  qAD
  
})

