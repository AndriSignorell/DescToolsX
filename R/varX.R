
#' (Weighted) Variance and Standard Deviation
#' 
#' \code{varX()} computes the variance of \code{x}, allowing the definition of
#' weights (unlike base R's \code{\link{var}()} function). Using the estimator
#' \code{ML} returns the uncorrected sample variance (which is a biased
#' estimator for the sample variance). \cr\code{sdX} yields the standard
#' deviation following the same logic.
#' 
#' Using estimator \code{"unbiased"} the denominator \eqn{n - 1} is used (known
#' as "Bessel's correction") which gives an unbiased estimator of the
#' (co)variance for i.i.d. observations.\cr \code{"ML"} yields the biased
#' version using the denominator \eqn{n}.
#' 
#' These functions return \code{\link{NA}} when there is only one observation,
#' and fail if \code{x} has length zero.
#' 
#' \bold{Note:}\verb{ } Analytic (precision) weights are not supported. For
#' likelihood-based weighted variance estimation, see
#' \code{\link[stats]{cov.wt}}.
#' 
#' @name varX
#' @aliases varX varX.default varS.Freq sdX
#' @param x a numeric vector, matrix or data frame.
#' @param estimator determines the estimator type; if \code{"unbiased"} (the
#' default) then the usual unbiased estimate (using \eqn{n - 1} as denominator)
#' is returned, if \code{"ML"} then it is the maximum likelihood estimate for a
#' Gaussian distribution (denominator \eqn{n}). Internally \code{\link{cov.wt}}
#' is used for both methods.
#' @param weights non-negative numeric vector of weights the same length as
#' \code{x} nterpreted as frequency (replication) weights. Observations with
#' larger weights contribute more strongly to the empirical distribution.
#' @param na.rm logical. Should missing values be removed?
#' @param breaks breaks for calculating the variance for classified data as
#' composed by \code{\link{Freq}}.
#' @param \dots further arguments passed to or from other methods.
#' @return single numeric value
#' @seealso \code{\link{varCI}} for confidence intervals and
#' \code{\link{varTest}} for tests.
#' 
#' base R's implementations \code{\link{var}}, \code{\link{sd}},
#' \code{\link{cov}}
#' 
#' also consider \code{\link{madX}}, the most robust alternative
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The
#' New S Language}.  Wadsworth & Brooks/Cole.
#' @keywords univar multivariate array
#' @examples
#' 
#' varX(1:10)                 # 9.166667
#' sdX(1:10)
#' 
#' varX(1:5, weights=1:5)     # 2.5
#' # same as c(1 2 2 3 3 3 4 4 4 4 5 5 5 5 5):
#' varX(rep(1:5, times=1:5))  # 2.5
#' 
#' # weighted Variance
#' set.seed(45)
#' (z <- as.numeric(names(w <- table(x <- sample(-10:20, size=50, replace=TRUE)))))
#' varX(z, weights=w)
#' sdX(z, weights=w)
#' 
#' # check!
#' all.equal(varX(x), varX(z, weights=w))
#' 
#' 
#' # Variance for frequency tables
#' varX(Freq(as.table(c(6,16,24,25,17))),
#'           breaks=c(0, 10, 20, 30, 40, 50))
#'      


#' @rdname varX
#' @export
sdX <- function (x, estimator = c("unbiased", "ML"),
                 weights = NULL, na.rm = FALSE, ...)
  sqrt(varX(if (is.vector(x) || is.factor(x)) x else as.double(x),
           estimator = estimator, weights=weights, na.rm = na.rm, ...))


#' @rdname varX
#' @export
varX <- function (x, ...)
  UseMethod("varX")



#' @rdname varX
#' @export
varX.default <- function(x, estimator = c("unbiased", "ML"),
                        weights = NULL, na.rm = FALSE, ...) {
  
  estimator <- match.arg(estimator)
  
  ## NA-Handling
  if (na.rm) {
    ok <- !is.na(x)
    x <- x[ok]
    if (!is.null(weights))
      weights <- weights[ok]
  }
  
  ## Weights?
  if (is.null(weights)) {
    res <- var(x=x, na.rm=na.rm)
    
    if(estimator == "ML")
      res <- res * ((n <- sum(ok)) - 1) / n
    
  } else {
    
    z <- .NormWeights(x, weights, na.rm = FALSE, zero.rm = TRUE)
    
    if (estimator == "ML"){
      res <- as.numeric(stats::cov.wt(cbind(z$x), z$weights, method = "ML")$cov)
      
    } else {
      
      xbar <- sum(z$weights * x) / z$wsum
      res <- sum(z$weights * ((z$x - xbar)^2))/(z$wsum - 1)
    }
    
  }
  
  return( res )
  
}



#' @rdname varX
#' @export
varX.Freq <- function(x, breaks, ...)  {
  n <- sum(x$freq)
  mu <- sum(head(MoveAvg(breaks, order=2, align="left"), -1) * x$perc)
  s2 <- (sum(head(MoveAvg(breaks, order=2, align="left"), -1)^2 * x$freq) - n*mu^2) / (n-1)
  
  return(s2)
}

