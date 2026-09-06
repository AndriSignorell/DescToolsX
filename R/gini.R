
#' Gini Coefficient
#'
#' Computes the Gini coefficient, a widely used measure of inequality,
#' optionally with bootstrap confidence intervals.
#'
#' The Gini coefficient ranges from 0 (perfect equality) to 1 (maximal
#' inequality). For finite samples, the uncorrected estimator is biased;
#' setting `unbiased = TRUE` applies a standard correction factor.
#'
#' Weights are interpreted as frequency (replication) weights, meaning that
#' each observation contributes proportionally to its weight in the empirical
#' distribution.
#'
#' Confidence intervals are obtained via bootstrap resampling using the
#' \pkg{boot} package. The default interval type is bias-corrected and
#' accelerated ("bca").
#'
#' @param x numeric vector of non-negative values
#' 
#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()].
#'
#' @param unbiased logical; whether to apply the small-sample bias
#' correction factor \eqn{n/(n-1)}, with \eqn{n} the effective sample size
#' (the sum of the weights). For unweighted data this is the usual
#' \eqn{1/(1 - \sum w_i^2)}.
#' @param weights optional non-negative numeric vector with the same length as
#' `x`
#' @param na.rm logical; whether to remove missing values before computation
#' @param \dots additional arguments passed to the bootstrap procedure:
#'   \describe{
#'     \item{`type`}{confidence interval type (default `"bca"`)}
#'     \item{`R`}{number of bootstrap replications (default 999)}
#'     \item{`parallel`}{parallelization mode (`"no"`,
#'       `"multicore"`, or `"snow"`)}
#'     \item{`ncpus`}{number of CPUs}
#'   }
#'
#' @return if `conf.level = NA`, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{`est`}{point estimate of the Gini coefficient}
#'   \item{`lci`}{lower confidence interval bound}
#'   \item{`uci`}{upper confidence interval bound}
#' }
#'
#' @details
#' The implementation uses a numerically stable formulation based on the Lorenz
#' curve:
#'
#' \deqn{
#' G = \frac{1}{\mu} \sum_i w_i x_i (2F_i - 1)
#' }
#'
#' where \eqn{F_i} are midpoints of cumulative weights and \eqn{\mu} is the
#' weighted mean.
#'
#' @examples
#' x <- c(10, 20, 30, 40)
#' gini(x)
#'
#' # weighted example
#' gini(c(10, 0), weights = c(2, 3))
#'
#' # with confidence interval
#' gini(x, conf.level = 0.95, R = 499)
#'
#' @details
#' `sides` names the side on which the finite bound lies: `"left"`
#' yields an interval bounded below, `"right"` one bounded above. The
#' coefficient is bounded, so the open side is reported at the range
#' boundary (0 or 1) rather than as \eqn{\pm\infty} (design_rules.md 4.1).
#'
#' @section Random number generation:
#' A confidence level triggers a bootstrap and therefore advances R's
#' global random number generator. Call [base::set.seed()]
#' beforehand for reproducible intervals.
#'
#' @family inequality
#' @concept inequality
#' @concept concentration-index
#' @export
gini <- function(x, 
                 conf.level = NA,
                 sides = c("two.sided", "left", "right"),
                 unbiased = TRUE,
                 weights = NULL,
                 na.rm = FALSE,
                 ...) {
  
  # --- weights ---
  if (is.null(weights)) {
    weights <- rep_len(1, length(x))
  }
  
  # --- NA handling ---
  if (na.rm) {
    keep <- !is.na(x) & !is.na(weights)
    x <- x[keep]
    weights <- weights[keep]
  }
  
  # --- checks ---
  if (length(x) == 0)
    stop("empty input")
  
  if (any(is.na(x)) || any(is.na(weights)))
    stop("missing values not allowed")
  
  if (any(x < 0))
    stop("x must be non-negative")
  
  if (any(weights < 0))
    stop("weights must be non-negative")
  
  # sum of weights must be > 0
  if (sum(weights) == 0) {
    if (is.na(conf.level))
      return(NA_real_)
    return(c(est = NA_real_, lci = NA_real_, uci = NA_real_))
  }
  
  
  # --- core gini ---
  .giniCore <- function(x, w, unbiased = FALSE) {

    o <- order(x)
    x <- x[o]
    w <- w[o]

    wsum <- sum(w)
    wn <- w / wsum

    cw <- cumsum(wn)
    # 'F' as a local name shadows the base alias for FALSE
    midF <- cw - wn / 2

    mu <- sum(wn * x)

    if (mu == 0)
      return(0)

    G <- sum(wn * x * (2 * midF - 1)) / mu

    # The correction used to be 1/(1 - sum(wn^2)) on the NORMALIZED
    # weights, which makes it depend on how the sample is expressed
    # rather than on its size: gini(c(10, 0), weights = c(2, 3)) gave
    # 1.25 - outside [0, 1] - while the equivalent replicated vector
    # c(0,0,0,10,10) gave 0.75. Weights are documented as frequency
    # weights, so the effective sample size is their sum, and n/(n-1)
    # reproduces the old (correct) value for unweighted data exactly.
    if (unbiased) {
      if (wsum <= 1)
        stop("the bias correction needs an effective sample size above 1")
      G <- G * wsum / (wsum - 1)
    }

    G
  }
  
  
  # --- no CI ---
  if (is.na(conf.level)) {
    return(.giniCore(x, weights, unbiased = unbiased))
  }
  
  
  # --- CI ---
  sides <- match.arg(sides)
  
  if (sides != "two.sided")
    conf.level <- 1 - 2 * (1 - conf.level)
  
  dots <- list(...)
  boot_args <- .extractBootArgs(dots)
  
  boot.fun <- boot::boot(
    data = x,
    statistic = function(z, i, u, unbiased)
      .giniCore(z[i], u[i], unbiased),
    R = boot_args$R,
    u = weights,
    unbiased = unbiased,
    parallel = boot_args$parallel,
    ncpus = boot_args$ncpus
  )
  
  ci <- boot::boot.ci(
    boot.fun,
    conf = conf.level,
    type = boot_args$type
  )
  
  # ci[[4]] happens to be the first interval component, but naming it is
  # both clearer and safe if boot.ci ever gains a component
  ciMat <- ci[[switch(boot_args$type,
                      norm = "normal", basic = "basic", stud = "student",
                      perc = "percent", bca = "bca")]]

  bounds <- if (boot_args$type == "norm") ciMat[2:3] else ciMat[4:5]

  # The one-sided case doubled alpha above and then did nothing with it:
  # gini(x, conf.level = 0.95, sides = "left") returned a two-sided 90%
  # interval labelled as one-sided. Gini is bounded, so the open side goes
  # to the range boundary.
  lci <- max(bounds[1L], 0)
  uci <- min(bounds[2L], 1)

  if (sides == "left")  uci <- 1
  if (sides == "right") lci <- 0

  c(est = unname(boot.fun$t0), lci = unname(lci), uci = unname(uci))
}
