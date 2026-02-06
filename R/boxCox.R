
#' Box Cox Transformation
#' 
#' \code{boxCox()} returns a transformation of the input variable using a
#' Box-Cox transformation.\cr \code{boxCoxInv()} reverses the transformation.
#' 
#' The Box-Cox transformation is given by
#' 
#' % \deqn{\theta(h) = % \left\{\begin{array}{ll} % u_\beta \left(\mu +
#' \frac{\nu(h)}{\Gamma(1 - \xi)} % \right),&\xi \neq 0\\ %
#' \exp\left(\frac{\nu(h)}{\sigma}\right),&\xi = 0 % \end{array}\right. %
#' }{u_beta (mu + nu(h) / Gamma(1 - \xi)), if \xi < 1, % exp(nu(h)/sigma),
#' otherwise}
#' 
#' \deqn{f_\lambda(x) = }{f(x;\lambda)=(x^\lambda - 1)/\lambda, if \lambda not
#' 0, log(x), otherwise}\deqn{ \left\{\begin{array}{ll}
#' }{f(x;\lambda)=(x^\lambda - 1)/\lambda, if \lambda not 0, log(x),
#' otherwise}\deqn{ \frac{x^\lambda - 1}{\lambda} &\textup{for }\lambda \neq
#' 0\\ }{f(x;\lambda)=(x^\lambda - 1)/\lambda, if \lambda not 0, log(x),
#' otherwise}\deqn{ log(x) &\textup{for }\lambda = 0 }{f(x;\lambda)=(x^\lambda
#' - 1)/\lambda, if \lambda not 0, log(x), otherwise}\deqn{ \end{array}\right.
#' }{f(x;\lambda)=(x^\lambda - 1)/\lambda, if \lambda not 0, log(x), otherwise}
#' 
#' @name boxCox
#' @aliases boxCox boxCoxInv
#' @param x a numeric vector
#' @param lambda transformation parameter
#' @return a numeric vector of the same length as x.
#' @note These two functions are borrowed from \code{library(forecast)}.
#' @author Rob J Hyndman <rob.hyndman@@monash.edu>
#' @seealso Use \code{\link{boxCoxLambda}} or \code{\link[MASS]{boxcox}} in
#' \code{library(MASS)} to find optimal lambda values.
#' @references Box, G. E. P. and Cox, D. R. (1964) An analysis of
#' transformations. \emph{JRSS B} \bold{26} 211--246.
#' @keywords univar
#' @examples
#' 
#' # example by Greg Snow
#' x <- rlnorm(500, 3, 2)
#' 
#' par(mfrow=c(2,2))
#' qqnorm(x, main="Lognormal")
#' qqnorm(boxCox(x, 1/2), main="boxCox(lambda=0.5)")
#' qqnorm(boxCox(x, 0), main="boxCox(lambda=0)")
#' 
#' plotFdist(boxCox(x, 0))
#' 
#' bx <- boxCox(x, lambda = boxCoxLambda(x) )
#' 


#' @rdname boxCox
#' @export
boxCox <- function (x, lambda) {
  
  # from library(forecast)
  
  # Author: Rob J Hyndman
  # origin: library(forecast)
  if (lambda < 0)
    x[x < 0] <- NA
  if (lambda == 0)
    out <- log(x)
  else out <- (sign(x) * abs(x)^lambda - 1)/lambda
  if (!is.null(colnames(x)))
    colnames(out) <- colnames(x)
  return(out)
  
  # Greg Snow's Variant
  # boxCox <- function (x, lambda)
  # {
  # ### Author: Greg Snow
  # ### Source: Teaching Demos
  # xx <- exp(mean(log(x)))
  # if (lambda == 0)
  # return(log(x) * xx)
  # res <- (x^lambda - 1)/(lambda * xx^(lambda - 1))
  # return(res)
  # }
  
}


#' @rdname boxCox
#' @export
boxCoxInv <- function(x, lambda){
  if (lambda < 0)
    x[x > -1/lambda] <- NA
  if (lambda == 0)
    out <- exp(x)
  else {
    xx <- x * lambda + 1
    out <- sign(xx) * abs(xx)^(1/lambda)
  }
  if (!is.null(colnames(x)))
    colnames(out) <- colnames(x)
  return(out)
}


