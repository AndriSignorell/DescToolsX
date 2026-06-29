
#' Gini Coefficient
#'
#' Computes the Gini coefficient, a widely used measure of inequality,
#' optionally with bootstrap confidence intervals.
#'
#' The Gini coefficient ranges from 0 (perfect equality) to 1 (maximal
#' inequality). For finite samples, the uncorrected estimator is biased;
#' setting \code{unbiased = TRUE} applies a standard correction factor.
#'
#' Weights are interpreted as frequency (replication) weights, meaning that
#' each observation contributes proportionally to its weight in the empirical
#' distribution.
#'
#' Confidence intervals are obtained via bootstrap resampling using the
#' \pkg{boot} package. The default interval type is bias-corrected and
#' accelerated ("bca").
#'
#' @param x Numeric vector of non-negative values.
#' @param conf.level Confidence level in (0, 1). If \code{NA} (default),
#'   no confidence interval is computed.
#' @param sides Character string specifying the interval type: \code{"two.sided"}
#'   (default), \code{"left"}, or \code{"right"}.
#' @param method Currently only \code{"boot"} is supported.
#' @param unbiased Logical. Apply bias correction factor \eqn{1/(1 - \sum w_i^2)}.
#' @param weights Optional non-negative numeric vector of the same length as \code{x}.
#' @param na.rm Logical. Remove missing values before computation.
#' @param \dots Additional arguments passed to the bootstrap procedure:
#'   \describe{
#'     \item{type}{Confidence interval type (default \code{"bca"})}
#'     \item{R}{Number of bootstrap replications (default 999)}
#'     \item{parallel}{Parallelization mode (\code{"no"}, \code{"multicore"}, \code{"snow"})}
#'     \item{ncpus}{Number of CPUs}
#'   }
#'
#' @return If \code{conf.level = NA}, a single numeric value. Otherwise a named
#'   vector with elements \code{est}, \code{lci}, and \code{uci}.
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

#' @family inequality  
#' @concept inequality  
#' @concept concentration-index
#'
#'


#' @export
gini <- function(x, 
                 conf.level = NA,
                 sides = c("two.sided", "left", "right"),
                 method = c("boot"),
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
  if (sum(weights) == 0)
    return(NA_real_)
  
  
  # --- core gini ---
  i.gini <- function(x, w, unbiased = FALSE) {
    
    o <- order(x)
    x <- x[o]
    w <- w[o]
    
    wsum <- sum(w)
    w <- w / wsum
    
    cw <- cumsum(w)
    F <- cw - w / 2
    
    mu <- sum(w * x)
    
    if (mu == 0)
      return(0)
    
    G <- sum(w * x * (2 * F - 1)) / mu
    
    if (unbiased)
      G <- G / (1 - sum(w^2))
    
    G
  }
  
  
  # --- no CI ---
  if (is.na(conf.level)) {
    return(i.gini(x, weights, unbiased = unbiased))
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
      i.gini(z[i], u[i], unbiased),
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
  
  if (boot_args$type == "norm") {
    res <- c(est = boot.fun$t0,
             lci = ci[[4]][2],
             uci = ci[[4]][3])
  } else {
    res <- c(est = boot.fun$t0,
             lci = ci[[4]][4],
             uci = ci[[4]][5])
  }
  
  res
}

