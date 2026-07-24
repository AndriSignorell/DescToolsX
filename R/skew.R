
#' Skewness
#' 
#' \code{skew} computes the skewness, \code{kurt} the excess kurtosis of the
#' values in x.
#' 
#' If \code{na.rm} is \code{TRUE} then missing values are removed before
#' computation proceeds. \cr
#' 
#' The estimator for calculating the skewness can either be:\cr \code{1: g_1 =
#' m_3 / m_2^(3/2) } \cr \code{2: G_1 = g_1 * sqrt(n(n-1)) / (n-2) }\cr
#' \code{3: b_1 = m_3 / s^3 = g_1 ((n-1)/n)^(3/2) } \cr
#' 
#' 1 is the typical definition used in Stata and in many older textbooks.  \cr
#' 2 is used in SAS and SPSS.  \cr 3 is used in MINITAB and BMDP. \cr
#' 
#' Cramer (1997) mentions the asymptotic standard error of the skewness:
#' \cr \preformatted{ASE.skew = sqrt( 6*n*(n-1)/((n-2)*(n+1)*(n+3)) )} to be
#' used for calculating the confidence intervals.  This is implemented here
#' with \code{method="classic"}. \cr However, Joanes and Gill (1998) advise
#' against this approach, pointing out that the normal assumptions would
#' virtually always be violated.  They suggest using the bootstrap method.
#' That's why the default method for the confidence interval type is set to
#' \code{"boot"}. If not further specified the boot ci type will be chosen as
#' \code{"bca"}.\cr
#' 
#' This implementation of the two functions is comparably fast, as the
#' expensive sums are coded in C++.
#' 
#' @name skew
#' 
#' @inheritParams ConfidenceIntervals
#' @param x a numeric vector. An object that is not a vector is coerced by
#' \code{as.vector} if possible.
#' @param estimator integer, either 1, 2 or 3 (default) defining the algorithm
#' used for calculation. See Details.
#' @param weights a numerical vector of weights the same length as \code{x}
#' giving the weights to use for elements of \code{x}
#' @param na.rm logical, indicating whether \code{NA} values should be stripped
#' before the computation proceeds. Defaults to \code{FALSE}.
#' @param \dots further arguments passed to \code{\link[boot]{boot}} when
#' confidence intervals are calculated
#' 
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{skewness estimate}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#' 
#' @references 
#' Cramer, D. (1997): \emph{Basic Statistics for Social Research}
#' Routledge.
#' 
#' Joanes, D. N., Gill, C. A. (1998): Comparing measures of sample skewness and
#' kurtosis. \emph{The Statistician}, 47, 183-189.
#' 
#' @family topic.dispersion
#' @concept shape
#' @concept moments
#' @concept descriptive-statistics
#' 
#' 
#' @examples
#' 
#' skew(Pizza$price, na.rm=TRUE)
#' 
#' # use sapply to calculate skewness for a data.frame
#' sapply(Pizza[,c("temperature","price","delivery_min")], skew, na.rm=TRUE)
#' 
#' # or apply to do that columnwise with a matrix
#' apply(as.matrix(Pizza[,c("temperature","price","delivery_min")]), 2, 
#'       skew, na.rm=TRUE)
#' 
#'
#' @seealso [meanX], [sdX], similar code in \pkg{e1071}
#' 
#' @family shape
#' @concept descriptive-statistics
#' @concept robust-statistics
#'
#'
#'
#' @export
skew <- function(x,
                  conf.level = NA,
                  sides = c("two.sided", "left", "right"),
                  method = c("boot", "classic"),
                  estimator = 3,
                  weights = NULL,
                  na.rm = FALSE,
                  ...) {
  
  if (!is.numeric(x))
    stop("'x' must be numeric")
  
  if (!is.null(weights) &&
      length(weights) != length(x))
    stop("'weights' must have same length as 'x'")
  
  if (!estimator %in% c(1, 2, 3))
    stop("'estimator' must be one of 1, 2, or 3")
  
  if (is.na(conf.level)) {
    
    res <- .skew(
      x,
      weights = weights,
      estimator = estimator,
      na.rm = na.rm
    )["est"]
    
  } else {
    
    sides <- match.arg(
      sides,
      choices = c("two.sided", "left", "right"),
      several.ok = FALSE
    )
    
    if (sides != "two.sided")
      conf.level <- 1 - 2 * (1 - conf.level)
    
    method <- match.arg(method)
    
    res <- switch(
      
      method,
      
      classic = .skew.classic(
        x,
        conf.level = conf.level,
        estimator = estimator,
        weights = weights,
        na.rm = na.rm
      ),
      
      boot = .skew.boot(
        x,
        conf.level = conf.level,
        estimator = estimator,
        weights = weights,
        na.rm = na.rm,
        ...
      )
    )
    
    if (sides == "left") {
      
      res[3] <- Inf
      
    } else if (sides == "right") {
      
      res[2] <- -Inf
    }
  }
  
  res
}


# == internal helper functions ================================================


.skew <- function(x,
                    weights = NULL,
                    estimator = 3,
                    na.rm = FALSE) {
  
  # C++ part for the expensive
  # (x - mean(x))^2 etc.
  # is roughly 14 times faster
  
  # estimator 1: older textbooks
  
  if (!is.null(weights)) {
    
    # use a standard treatment for weights
    z <- .normWeights(
      x,
      weights,
      na.rm = na.rm
    )
    
    r.skew <- skew_weighted_cpp(
      as.numeric(z$x),
      as.numeric(meanX(
        z$x,
        weights = z$weights
      )),
      as.numeric(z$weights)
    )
    
    n <- z$wsum
    
  } else {
    
    if (na.rm)
      x <- na.omit(x)
    
    r.skew <- skew_cpp(
      as.numeric(x),
      as.numeric(mean(x))
    )
    
    n <- length(x)
  }
  
  se <- sqrt(
    (6 * (n - 2)) /
      ((n + 1) * (n + 3))
  )
  
  if (estimator == 2) {
    
    # estimator 2: SAS/SPSS
    
    r.skew <- r.skew *
      sqrt(n) *
      sqrt(n - 1) /
      (n - 2)
    
    se <- se *
      sqrt(n * (n - 1)) /
      (n - 2)
    
  } else if (estimator == 3) {
    
    # estimator 3: MINITAB/BDMP
    
    r.skew <- r.skew *
      ((n - 1) / n)^(3 / 2)
    
    se <- se *
      ((n - 1) / n)^(3 / 2)
  }
  
  c(
    est = r.skew,
    var = se^2
  )
}


.skew.classic <- function(x,
                           conf.level,
                           estimator = 3,
                           weights = NULL,
                           na.rm = FALSE) {
  
  res <- .skew(
    x,
    weights = weights,
    estimator = estimator,
    na.rm = na.rm
  )
  
  c(
    est = unname(res["est"]),
    lci = unname(qnorm((1 - conf.level) / 2) *
      sqrt(res["var"])),
    uci = unname(qnorm(1 - (1 - conf.level) / 2) *
      sqrt(res["var"]))
  )
}


.skew.boot <- function(x,
                        conf.level,
                        estimator = 3,
                        weights = NULL,
                        na.rm = FALSE,
                        ...) {
  
  # Problematic standard errors and confidence intervals
  # for skewness and kurtosis.
  #
  # Wright DB, Herrington JA. (2011)
  # recommend only bootstrap intervals.
  #
  # adjusted bootstrap percentile (BCa) interval
  
  args <- .extractBootArgs(list(...))
  
  boot.fun <- boot::boot(
    
    x,
    
    function(x, d)
      .skew(
        x[d],
        weights = weights,
        estimator = estimator
      ),
    
    R        = args$R,
    parallel = args$parallel,
    ncpus    = args$ncpus
  )
  
  ci <- boot::boot.ci(
    boot.fun,
    conf = conf.level,
    type = args$type
  )
  
  if (args$type == "norm") {
    
    c(
      est = unname(boot.fun$t0[1]),
      lci = unname(ci[[4]][2]),
      uci = unname(ci[[4]][3])
    )
    
  } else {
    
    c(
      est = unname(boot.fun$t0[1]),
      lci = unname(ci[[4]][4]),
      uci = unname(ci[[4]][5])
    )
  }
}

