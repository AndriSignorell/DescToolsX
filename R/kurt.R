
#' Kurtosis
#' 
#' `kurt()` returns the excess kurtosis, therefore the kurtosis calculates
#' as `kurt(x) + 3` if required.
#' 
#' If `na.rm` is `TRUE` then missing values are removed before
#' computation proceeds. \cr
#' 
#' The estimator for calculating kurtosis can either be:\cr 
#' `1: g_2 = m_4 / m_2^2 - 3 ` \cr
#' `2: G_2 = ((n+1) g_2 + 6) * (n-1) / ((n-2)(n-3)) ` \cr 
#' `3: b_2 = m_4 / s^4 - 3 = (g_2 + 3) (1 - 1/n)^2 - 3 ` \cr
#' 
#' 1 is the typical definition used in Stata and in many older textbooks.  \cr
#' 2 is used in SAS and SPSS.  \cr 3 is used in MINITAB and BMDP. \cr
#' 
#' `method = "classic"` uses a Wald interval with the standard error of the
#' kurtosis under normality. For estimator 1 this is the exact standard
#' error of \eqn{g_2}
#' \preformatted{SE(g_2) = sqrt(24*n*(n - 2)*(n - 3) / ((n + 1)^2*(n + 3)*(n + 5)))}
#' and since estimators 2 and 3 are linear in \eqn{g_2}, their standard
#' errors follow by multiplying with the respective slope,
#' \eqn{(n-1)(n+1)/((n-2)(n-3))} and \eqn{((n-1)/n)^2}. For estimator 2
#' this gives the familiar formula of Cramer (1997), as reported by SPSS:
#' \preformatted{SES = sqrt(6*n*(n - 1) / ((n - 2)*(n + 1)*(n + 3)))
#' SEK = 2 * SES * sqrt((n^2 - 1) / ((n - 3)*(n + 5)))
#'     = sqrt(24*n*(n - 1)^2 / ((n - 3)*(n - 2)*(n + 3)*(n + 5)))}
#' Note that it applies to \eqn{G_2} only, not to the default estimator 3.
#' With weights, \eqn{n} is the sum of the weights. \cr
#' However, Joanes and Gill (1998) advise
#' against this approach, pointing out that the normal assumptions would
#' virtually always be violated.  They suggest using the bootstrap method.
#' That's why the default method for the confidence interval type is set to
#' `"boot"`. 
#' If not further specified the boot ci type will be chosen as `"bca"`.\cr
#' 
#' This implementation is comparably fast, as the expensive sums are coded in C.
#' 
#' @name kurt
#' 
#' @param x a numeric vector. An object that is not a vector is coerced by
#' `as.vector` if possible.
#' @param estimator integer, either 1, 2 or 3 (default) defining the algorithm
#' used for calculation. See Details.
#' @param weights a numerical vector of weights the same length as `x`
#' giving the weights to use for elements of `x`
#' 
#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()].
#' @param method character string specifying the confidence interval method.
#'   `"boot"` (default) uses a nonparametric bootstrap, with BCa
#'   intervals unless another bootstrap type is supplied through `\dots`;
#'   `"classic"` uses a Wald interval based on the asymptotic standard
#'   error. See Details and [ConfidenceIntervals()].
#'   
#' @param na.rm logical, indicating whether `NA` values should be stripped
#' before the computation proceeds. Defaults to `FALSE`.
#' @param \dots further arguments passed to [boot::boot()] when
#' confidence intervals are calculated
#' 
#' @return if `conf.level = NA`, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{`est`}{kurtosis estimate}
#'   \item{`lci`}{lower confidence interval bound}
#'   \item{`uci`}{upper confidence interval bound}
#' }
#' 
#' @references Cramer, D. (1997): *Basic Statistics for Social Research*
#' Routledge.
#' 
#' Joanes, D. N., Gill, C. A. (1998): Comparing measures of sample skewness and
#' kurtosis. *The Statistician*, 47, 183-189.
#' 
#' @examples
#' 
#' kurt(Pizza$price, na.rm=TRUE)
#' 
#' # use sapply to calculate skewness for a data.frame
#' sapply(Pizza[,c("temperature","price","delivery_min")], kurt, na.rm=TRUE)
#' 
#' # or apply to do that columnwise with a matrix
#' apply(as.matrix(Pizza[,c("temperature","price","delivery_min")]), 2, 
#'       kurt, na.rm=TRUE)
#' 
#' @seealso [meanX], [sdX], similar code in \pkg{e1071}
#'
#' @section Random number generation:
#' `method = "boot"` - the default - resamples and therefore advances
#' R's global random number generator. Call [base::set.seed()]
#' beforehand for reproducible intervals.
#'
#' @family shape
#' @concept descriptive-statistics
#' @concept robust-statistics
#' @export
kurt <- function(x,
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
    
    # unname(): the documented return is "a numeric scalar", but the
    # subsetting carried the name "est" through
    res <- unname(.i.kurt(
      x,
      weights = weights,
      na.rm = na.rm,
      estimator = estimator
    )[["est"]])
    
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
      
      classic = .kurt.classic(
        x,
        conf.level = conf.level,
        estimator = estimator,
        weights = weights,
        na.rm = na.rm
      ),
      
      boot = .kurt.boot(
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



.i.kurt <- function(x,
                    weights = NULL,
                    na.rm = FALSE,
                    estimator = 3) {
  
  # estimator 1: older textbooks
  
  if (!is.null(weights)) {
    
    # use a standard treatment for weights
    z <- .normWeights(
      x,
      weights,
      na.rm = na.rm
    )
    
    r.kurt <- kurt_weighted_cpp(
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
    
    r.kurt <- kurt_cpp(
      as.numeric(x),
      as.numeric(mean(x))
    )
    
    n <- length(x)
  }
  
  # exact standard error of g_2 under normality; estimators 2 and 3 are
  # linear in g_2 and scale it by their slope below. For estimator 2 this
  # reproduces Cramer's (1997) sqrt(24n(n-1)^2 / ((n-3)(n-2)(n+3)(n+5))),
  # i.e. 2 * SES * sqrt((n^2-1) / ((n-3)(n+5))).
  # defined from n = 4 on: at n = 3 the radicand is 0 and estimator 2
  # divides by n - 3, so the interval would be degenerate or NaN
  se <- if (is.na(n) || n < 4) NA_real_ else sqrt(
    (24 * n * (n - 2) * (n - 3)) /
      ((n + 1)^2 * (n + 3) * (n + 5))
  )
  
  if (estimator == 2) {
    
    # estimator 2: SAS/SPSS
    
    r.kurt <- (
      ((r.kurt + 3) * (n + 1) / (n - 1) - 3) *
        (n - 1)^2 / (n - 2) / (n - 3)
    )
    
    se <- se * (
      ((n - 1) * (n + 1)) /
        ((n - 2) * (n - 3))
    )
    
  } else if (estimator == 3) {
    
    # estimator 3: MINITAB/BMDP
    
    r.kurt <- (r.kurt + 3) * (1 - 1 / n)^2 - 3
    
    se <- se * ((n - 1) / n)^2
  }
  
  c(
    est = r.kurt,
    var = se^2
  )
}


.kurt.classic <- function(x,
                           conf.level,
                           estimator = 3,
                           weights = NULL,
                           na.rm = FALSE) {
  
  res <- .i.kurt(
    x,
    weights = weights,
    na.rm = na.rm,
    estimator = estimator
  )
  
  est <- unname(res[["est"]])
  se  <- sqrt(unname(res[["var"]]))

  # The estimate was MISSING from both bounds. The former lines read
  #
  #     lci = qnorm((1 - conf.level)/2)     * sqrt(res["var"])
  #     uci = qnorm(1 - (1 - conf.level)/2) * sqrt(res["var"])
  #
  # i.e. -z*se and +z*se: an interval centred on ZERO rather than on the
  # kurtosis. Excess kurtosis often sits near zero, so the result looked
  # plausible - but for a sample with est = 1.8 and se = 0.4 the reported
  # interval was (-0.78, 0.78), which does not even contain the estimate
  # printed beside it.
  c(
    est = est,
    lci = est - qnorm(1 - (1 - conf.level) / 2) * se,
    uci = est + qnorm(1 - (1 - conf.level) / 2) * se
  )
}


.kurt.boot <- function(x,
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

  # na.rm was accepted and then ignored: x was never filtered and the
  # statistic below called .i.kurt() without it, so kurt(x, conf.level =,
  # na.rm = TRUE) on data with NAs produced NA replicates and a failing
  # boot.ci(). Filter once, here, keeping weights aligned.
  if (na.rm) {
    keep <- !is.na(x)
    x <- x[keep]
    if (!is.null(weights)) weights <- weights[keep]
  }

  boot.fun <- boot::boot(

    x,

    # weights[d], not weights: the resample takes observation d[i] but was
    # handed the weight of observation i, so every replicate paired values
    # with the wrong weights. Unweighted calls were unaffected, which is
    # why it survived. Returning only est also keeps boot()'s statistic
    # one-dimensional - it used to get c(est, var) and silently bootstrap
    # the variance alongside.
    function(x, d)
      .i.kurt(
        x[d],
        weights = if (is.null(weights)) NULL else weights[d],
        estimator = estimator
      )[["est"]],

    R        = args$R,
    parallel = args$parallel,
    ncpus    = args$ncpus
  )
  
  ci <- boot::boot.ci(
    boot.fun,
    conf = conf.level,
    type = args$type
  )
  
  # by name rather than by position, as in gini() and hodgesLehmann()
  ciMat <- ci[[switch(args$type,
                      norm = "normal", basic = "basic", stud = "student",
                      perc = "percent", bca = "bca")]]

  bounds <- if (args$type == "norm") ciMat[2:3] else ciMat[4:5]

  c(
    est = unname(boot.fun$t0[1]),
    lci = unname(bounds[1L]),
    uci = unname(bounds[2L])
  )
}


