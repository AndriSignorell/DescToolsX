
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
#' Cramer (1997) mentions the asymptotic standard error of \code{G_1}, that is
#' of \code{estimator = 2}:
#' \cr \preformatted{ASE.skew = sqrt( 6*n*(n-1)/((n-2)*(n+1)*(n+3)) )} to be
#' used for calculating the confidence intervals. The standard errors of the
#' other two estimators follow from it by the same factors that relate the
#' estimators themselves, so that \code{est/se} does not depend on the choice
#' of \code{estimator}. This is implemented here with \code{method="classic"}.
#' \cr However, Joanes and Gill (1998) advise
#' against this approach, pointing out that the normal assumptions would
#' virtually always be violated.  They suggest using the bootstrap method.
#' That's why the default method for the confidence interval type is set to
#' \code{"boot"}. If not further specified the boot ci type will be chosen as
#' \code{"bca"}.\cr
#' 
#' The standard error is only defined for \code{n >= 3}; for shorter input the
#' variance, and with it any \code{method="classic"} interval, is \code{NA}.
#' 
#' This implementation of the two functions is comparably fast, as the
#' expensive sums are coded in C++.
#' 
#' @name skew
#' 
#' @inheritParams ConfidenceIntervals
#' @param x a numeric vector
#' @param estimator integer, either 1, 2 or 3 (default) defining the algorithm
#' used for calculation. See Details.
#' @param weights a numerical vector of weights the same length as \code{x}
#' giving the weights to use for elements of \code{x}. The weights are read as
#' frequencies, so that their sum takes the place of the sample size in the
#' estimator's bias corrections and in the standard error.
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
#' @family shape
#' @concept moments
#' 
#' 
#' @examples
#' 
#' skew(bedrock::Pizza$price, na.rm=TRUE)
#' 
#' # use sapply to calculate skewness for a data.frame
#' sapply(bedrock::Pizza[,c("temperature","price","delivery_min")], 
#'        skew, na.rm=TRUE)
#' 
#' # the estimate lies inside its own confidence interval
#' set.seed(1)
#' skew(rlnorm(50), conf.level=0.95, method="classic")
#' 
#'
#' @seealso [meanX], [sdX], similar code in \pkg{e1071}
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
  
  if (!is.null(weights)) {
    
    if (!is.numeric(weights))
      stop("'weights' must be numeric")
    
    if (length(weights) != length(x))
      stop("'weights' must have same length as 'x'")
  }
  
  # length(estimator) is checked as well: %in% is vectorised, so a vector of
  # length 2 would have produced a condition of length > 1 here.
  if (length(estimator) != 1L || !isTRUE(estimator %in% c(1, 2, 3)))
    stop("'estimator' must be one of 1, 2, or 3")
  
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("'na.rm' must be a single non-missing logical value")
  
  # conf.level is the switch between the two return shapes, so it has to be a
  # single value before it can be tested with is.na(); NULL or a vector would
  # otherwise fail inside if() with an unrelated message.
  if (length(conf.level) != 1L)
    stop("'conf.level' must be a single value, or NA")
  
  if (is.na(conf.level)) {
    
    res <- .skew(
      x,
      weights = weights,
      estimator = estimator,
      na.rm = na.rm
    )["est"]
    
  } else {
    
    if (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1)
      stop("'conf.level' must be a single number in (0, 1), or NA")
    
    sides <- match.arg(sides)
    
    method <- match.arg(method)
    
    if (sides != "two.sided")
      conf.level <- 1 - 2 * (1 - conf.level)
    
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
    
    # Skewness is unbounded in both directions, so the open side is reported
    # as infinite rather than clipped to a range boundary.
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
    
    if (anyNA(z$x))
      return(c(est = NA_real_, var = NA_real_))
    
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
  
  # The asymptotic standard error is only defined from n = 3 on: below that
  # the radicand is negative and the estimator corrections divide by n - 2.
  # Returning NA here is what keeps .skew.classic() from reporting a NaN
  # interval next to a perfectly finite estimate.
  if (is.na(n) || n < 3) {
    
    se <- NA_real_
    
  } else {
    
    se <- sqrt(
      (6 * (n - 2)) /
        ((n + 1) * (n + 3))
    )
  }
  
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
    est = unname(r.skew),
    var = unname(se^2)
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
  
  est <- unname(res["est"])
  se  <- unname(sqrt(res["var"]))
  
  # The Wald interval is centred on the estimate. Without the est term the
  # bounds are symmetric about zero and need not contain the value they are
  # reported next to.
  c(
    est = est,
    lci = est + qnorm((1 - conf.level) / 2) * se,
    uci = est + qnorm(1 - (1 - conf.level) / 2) * se
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
  
  # Missing values are removed once, here, rather than inside the statistic:
  # replicates of differing length would each be a sample from a different
  # sample size, and boot() would resample the NAs along with the data.
  if (na.rm) {
    
    ok <- !is.na(x)
    
    if (!is.null(weights))
      ok <- ok & !is.na(weights)
    
    x <- x[ok]
    
    if (!is.null(weights))
      weights <- weights[ok]
  }
  
  boot.fun <- boot::boot(
    
    x,
    
    # weights[d] rather than weights: the indices d resample x, and a weight
    # belongs to its observation. Passing the unpermuted vector pairs replicate
    # i with the weight of the original observation i.
    function(x, d)
      .skew(
        x[d],
        weights = if (is.null(weights)) NULL else weights[d],
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
  
  bounds <- .bootCIBounds(ci, args$type)
  
  c(
    est = unname(boot.fun$t0[1]),
    lci = unname(bounds[1]),
    uci = unname(bounds[2])
  )
}
