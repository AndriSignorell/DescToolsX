
#' Finds many (all) roots of one equation within an interval
#' 
#' The function \code{unirootAll} searches the interval from lower to upper for
#' several roots (i.e., zero's) of a function \code{f} with respect to its
#' first argument.
#' 
#' \code{f} will be called as \code{f(x, ...)} for a numeric value of \code{x}.
#' 
#' Run \code{demo(Jacobandroots)} for an example of the use of
#' \code{unirootAll} for steady-state analysis.
#' 
#' See also second example of \code{gradient} This example is discussed in the
#' book by Soetaert and Herman (2009).
#' 
#' @param f the function for which the root is sought.
#' @param interval a vector containing the end-points of the interval to be
#' searched for the root.
#' @param lower the lower end point of the interval to be searched.
#' @param upper the upper end point of the interval to be searched.
#' @param tol the desired accuracy (convergence tolerance).
#' @param maxiter the maximum number of iterations.
#' @param n number of subintervals in which the root is sought.
#' @param ... additional named or unnamed arguments to be passed to \code{f}
#' (but beware of partial matching to other arguments).
#' @return a vector with the roots found in the interval
#' @note This is a verbatim copy from rootSolve::uniroot.all (v. 1.7).
#' @author Karline Soetaert <karline.soetaert@@nioz.nl>
#' @seealso \code{\link{uniroot}} for more information about input.
#' @keywords optimize
#' @examples
#' 
#' ## =======================================================================
#' ##   Mathematical examples
#' ## =======================================================================
#' 
#' # a well-behaved case...
#' fun <- function (x) cos(2*x)^3
#' 
#' curve(fun(x), 0, 10,main = "unirootAll")
#' 
#' All <- unirootAll(fun, c(0, 10))
#' points(All, y = rep(0, length(All)), pch = 16, cex = 2)
#' 
#' # a difficult case...
#' f <- function (x) 1/cos(1+x^2)
#' AA <- unirootAll(f, c(-5, 5))
#' curve(f(x), -5, 5, n = 500, main = "unirootAll")
#' points(AA, rep(0, length(AA)), col = "red", pch = 16)
#' 
#' f(AA)  # !!!
#' 
#' 
#' ## =======================================================================
#' ## Vectorisation:
#' ## =======================================================================
#' # from R-help Digest, Vol 130, Issue 27
#' # https://stat.ethz.ch/pipermail/r-help/2013-December/364799.html
#' 
#' integrand1 <- function(x) 1/x*dnorm(x)
#' integrand2 <- function(x) 1/(2*x-50)*dnorm(x)
#' integrand3 <- function(x, C) 1/(x+C)
#' 
#' res <- function(C) {
#'   integrate(integrand1, lower = 1, upper = 50)$value +
#'   integrate(integrand2, lower = 50, upper = 100)$value -
#'   integrate(integrand3, C = C, lower = 1, upper = 100)$value
#' }
#' 
#' # uniroot passes one value at a time to the function, so res can be used as such
#' uniroot(res, c(1, 1000))
#' 
#' # Need to vectorise the function to use unirootAll:
#' res <- Vectorize(res)
#' unirootAll(res, c(1,1000))
#' 
#' 
  

## =============================================================================
## uniroot.all: multiple roots of one nonlinear equation
## =============================================================================

#' @export
unirootAll <- function (f, interval, lower= min(interval),
                        upper= max(interval), tol= .Machine$double.eps^0.5,
                        maxiter= 1000, n = 100, ... ) {
  
  # this is a copy of rootSolve::uniroot.all v. 1.8.2.1
  # author: Karline Soetaert
  
  
  ## error checking as in uniroot...
  if (!missing(interval) && length(interval) != 2)
    stop("'interval' must be a vector of length 2")
  if (!is.numeric(lower) || !is.numeric(upper) || lower >=
      upper)
    stop("lower < upper  is not fulfilled")
  
  ## subdivide interval in n subintervals and estimate the function values
  xseq <- seq(lower, upper, len=n+1)
  #   changed in 0.99.36 5.5.2020
  # but we should maybe vectorize the functions in order to allow the user not to
  # bother about internal applies
  # ... not sure about the impact..
  
  # Original: mod  <- f(xseq, ...)
  mod  <- Vectorize(f)(xseq, ...)
  
  ## some function values may already be 0
  Equi <- xseq[which(mod==0)]
  
  ss   <- mod[1:n]*mod[2:(n+1)]  # interval where function values change sign
  ii   <- which(ss<0)
  
  for (i in ii)
    Equi <- c(Equi, uniroot(f, lower=xseq[i], upper=xseq[i+1], 
                            maxiter = maxiter, tol = tol, ...)$root)
  
  return(Equi)
  
  
}


