
#' Safe (Generalized) Huber M-Estimator of Location
#'
#' A (generalized) Huber M-estimator of location with MAD scale that handles
#' the degenerate case of zero scale gracefully, where
#' [MASS::huber()] would return an error.
#'
#' @details
#' **Wald interval** (`method = "wald"`)
#'
#' Uses an empirical sandwich standard error for the Huber M-estimator:
#' \deqn{
#'   \hat\mu \;\pm\; t_{1-\alpha/2,\,n-1}
#'   \hat s \sqrt{\frac{\hat\tau}{n}},
#' }
#' where
#' \deqn{
#'   \hat\tau =
#'   \frac{n \sum_{i=1}^n \psi_k(r_i)^2}
#'        {\left\{\sum_{i=1}^n I(|r_i| \leq k)\right\}^2},
#'   \qquad
#'   r_i = \frac{x_i-\hat\mu}{\hat s}.
#' }
#' Here, \eqn{\psi_k} denotes the Huber score function. No additional
#' small-sample adjustment is applied beyond use of the t-quantile.
#' 
#' **Bootstrap interval** (`method = "boot"`)
#'
#' The statistic \eqn{\hat\mu} is resampled \eqn{R} times.  Note that
#' `mu` and `s` are fixed at their initial values (computed from
#' the full data before resampling) and are not re-estimated on each
#' resample.  The bootstrap therefore targets the variability of the
#' location estimator with fixed scale initialization, not a fully
#' re-estimated robust estimator.  Bootstrap
#' arguments are passed through `...` and extracted via
#' `.extractBootArgs()`:
#' \describe{
#'   \item{`R`}{number of bootstrap replicates (default `999`)}
#'   \item{`type`}{confidence interval type: `"perc"`, `"basic"`,
#'     `"norm"` or `"bca"` (default)}
#'   \item{`parallel`}{parallelization mode: `"no"`,
#'     `"multicore"`, or `"snow"` (default `"no"`)}
#'   \item{`ncpus`}{number of CPUs for parallel bootstrap (default
#'     `getOption("boot.ncpus", 1L)`)}
#' }
#'
#' The original internal estimator is accessible as
#' `DescToolsX:::.huberM`.
#'
#' @param x numeric vector of data values
#' 
#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()]. The location is unbounded,
#'   so the open side is \eqn{\pm}`Inf`. One-sided intervals require
#'   `conf.level > 0.5`.
#'
#' @param method confidence interval method: `"wald"` (default) or
#' `"boot"`.
#' 
#' @param k positive tuning constant; the algorithm winsorizes at `k`
#' standard deviations. Default is `1.345`.
#' @param mu initial location estimate. `NULL` (default)
#'   uses `median(x)`, computed after `na.rm` is applied.
#' @param s scale estimate held constant through the iterations.
#'   `NULL` (default) uses `mad(x, center = mu)`, computed
#'   after `na.rm` is applied.
#' @param weights optional non-negative frequency weights of the same
#'   length as `x`. The starting values then become the weighted median
#'   and the weighted MAD (both via [medianX()], the MAD with the usual
#'   constant 1.4826), and \eqn{n} in the Wald interval is
#'   `sum(weights)`, so integer weights give exactly the result of the
#'   replicated data. The bootstrap resamples observations together with
#'   their weights.
#' @param na.rm logical; whether to remove missing values before computation;
#' default is `FALSE`
#' @param ... further arguments passed to the bootstrap engine when
#'   `method = "boot"`: `R`, `type`, `parallel`,
#'   and `ncpus`; see Details
#'
#' @return if `conf.level = NA`, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{`est`}{location estimate from Huber's M-estimator}
#'   \item{`lci`}{lower confidence interval bound}
#'   \item{`uci`}{upper confidence interval bound}
#' }
#'
#' @note Adapted from code by Martin Maechler to conform to package standards
#'
#' @references
#' Huber, P. J. and Ronchetti, E. M. (2009).
#' *Robust Statistics*, 2nd ed. Wiley.
#' 
#' @seealso [MASS::huber()], [mad()],
#'   [tukeyBiweight()]
#'
#' @section Random number generation:
#' `method = "boot"` resamples and therefore advances R's global
#' random number generator. Call [base::set.seed()] beforehand
#' for reproducible intervals.
#'
#' @examples
#' huberM(c(1:9, 1000))
#' mad(c(1:9, 1000))
#'
#' set.seed(7)
#' x <- c(round(rnorm(1000), 1), round(rnorm(50, mean = 10, sd = 10)))
#'
#' huberM(x, conf.level = 0.95)
#' huberM(x, conf.level = 0.95, method = "boot", R = 499, type = "bca")
#'
#' # degenerate case: scale zero
#' huberM(rep(9, 100))
#'
#' @family location
#' @concept location
#' @concept robust-statistics
#' @export
huberM <- function(x,
                   conf.level = NA,
                   sides      = c("two.sided", "left", "right"),
                   method     = c("wald", "boot"),
                   k          = 1.345,
                   mu         = NULL,
                   s          = NULL,
                   weights    = NULL,
                   na.rm      = FALSE,
                   ...) {
  
  # --- input checks --------------------------------------------------
  if (!is.numeric(x) || length(x) == 0L)
    stop("Argument 'x' must be a non-empty numeric vector.")
  
  if (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k <= 0)
    stop("Argument 'k' must be a single positive finite number.")

  # up front, not after the NA return: conf.level used to be read by
  # is.na() unchecked, so NULL or c(0.9, 0.95) broke the if() internally
  # and NaN silently meant "no interval"
  checkConfLevel(conf.level)
  sides  <- match.arg(sides)
  method <- match.arg(method)
  checkFlag(na.rm)

  if (!is.na(conf.level) && sides != "two.sided" && conf.level <= 0.5)
    stop("Argument 'conf.level' must exceed 0.5 for a one-sided interval.")

  if (!is.null(weights)) {
    if (!is.numeric(weights) || length(weights) != length(x) ||
        any(!is.finite(weights)) || any(weights < 0))
      stop("Argument 'weights' must be finite, non-negative and as long as 'x'.")
  }
  
  if (na.rm) {
    keep <- !is.na(x)
    x <- x[keep]
    # the weights follow the observations they belong to
    if (!is.null(weights)) weights <- weights[keep]
  }
  
  if (anyNA(x)) {
    if (is.na(conf.level))
      return(NA_real_)
    return(c(est = NA_real_, lci = NA_real_, uci = NA_real_))
  }

  if (!is.null(weights) && sum(weights) <= 0)
    stop("Argument 'weights' must not sum to zero.")
  
  # --- defaults for mu / s after NA removal --------------------------
  # The weighted defaults use the suite's weighted median, which for
  # frequency weights reproduces median() and mad() of the replicated data.
  # (Maechler's original took the weighted HIGH median here, and for s
  # without the 1.4826 consistency constant - with equal weights the scale
  # was then a third smaller than mad() and the estimate changed.)
  if (is.null(mu)) {
    mu <- if (is.null(weights)) median(x) else medianX(x, weights = weights)
  } else {
    if (!is.numeric(mu) || length(mu) != 1L || !is.finite(mu))
      stop("Argument 'mu' must be a single finite numeric value.")
  }
  
  if (is.null(s)) {
    s <- if (is.null(weights)) mad(x, center = mu)
         else 1.4826 * medianX(abs(x - mu), weights = weights)
  } else {
    if (!is.numeric(s) || length(s) != 1L || !is.finite(s) || s < 0)
      stop("Argument 's' must be a single non-negative finite numeric value.")
  }
  
  # --- point estimate only -------------------------------------------
  if (is.na(conf.level))
    return(.huberM(x = x, k = k, weights = weights, mu = mu, s = s,
                   warn0scale = TRUE)$mu)
  
  # --- CI ------------------------------------------------------------
  # Two-sided at the adjusted level; applySides() opens the other side.
  confAdj <- if (sides != "two.sided") 2 * conf.level - 1 else conf.level
  alpha   <- 1 - confAdj
  nEff    <- if (is.null(weights)) length(x) else sum(weights)
  
  ci <- switch(method,
                
                wald = {
                  hm  <- .huberM(x = x, k = k, weights = weights,
                                 mu = mu, s = s, se = TRUE, warn0scale = TRUE)

                  # hm$SE, not a second hand-rolled computation. The
                  # former line called .tauHuber(x, hm$mu, k = k) without
                  # passing s, so tau fell back to its default s = mad(x)
                  # while the interval was scaled by hm$s. Those agree
                  # only for the default mu and s; supply either argument
                  # and the residuals entering tau are standardized by a
                  # different scale than the one multiplying them.
                  # .huberM(se = TRUE) already returns exactly
                  # s * sqrt(tau(x, mu, s, k) / n) - it was computed and
                  # then discarded.
                  est <- hm$mu
                  est + c(-1, 1) * qt(1 - alpha / 2, nEff - 1) * hm$SE
                },
                
                boot = {
                  dots      <- list(...)
                  boot_args <- .extractBootArgs(dots)
                  
                  # freeze mu/s/k - not re-estimated per resample (see
                  # @details); weights are resampled with their
                  # observations
                  k_  <- k
                  mu_ <- mu
                  s_  <- s
                  w_  <- weights
                  
                  boot_obj <- boot::boot(
                    x,
                    statistic = function(x, d)
                      .huberM(x = x[d], k = k_,
                              weights = if (is.null(w_)) NULL else w_[d],
                              mu = mu_, s = s_, warn0scale = FALSE)$mu,
                    R        = boot_args$R,
                    sim      = "ordinary",
                    parallel = boot_args$parallel,
                    ncpus    = boot_args$ncpus
                  )
                  
                  ci_obj <- boot::boot.ci(boot_obj,
                                          conf = confAdj,
                                          type = boot_args$type)
                  
                  # boot.ci slot names by type:
                  #   "norm"  -> $normal  (cols: level, lci, uci)
                  #   "basic" -> $basic   (cols: level, ?, ?, lci, uci)
                  #   "perc"  -> $percent (cols: level, ?, ?, lci, uci)
                  #   "bca"   -> $bca     (cols: level, ?, ?, lci, uci)
                  ci_mat <- switch(boot_args$type,
                                   norm  = ci_obj$normal,
                                   basic = ci_obj$basic,
                                   perc  = ci_obj$percent,
                                   bca   = ci_obj$bca
                  )
                  
                  if (is.null(ci_mat))
                    stop(
                      "boot.ci() returned NULL for type = '", boot_args$type, "'. ",
                      "Try a different 'type', increase 'R', or use method = 'wald'.",
                      call. = FALSE
                    )
                  
                  est <- unname(boot_obj$t0)
                  unname(if (boot_args$type == "norm") ci_mat[2:3] else ci_mat[4:5])
                }
  )
  
  c(est = est, applySides(ci, sides, lo = -Inf, hi = Inf))
}




# == internal helper functions ========================================


##  A modified "safe" (and more general) Huber estimator. Called by huberM(),
##  which supplies mu and s, so the defaults below apply only to direct calls.
.huberM <-
  function(x, k = 1.345, weights = NULL,
           tol = 1e-06,
           mu = if(is.null(weights)) median(x) else medianX(x, weights = weights),
           s = if(is.null(weights)) mad(x, center=mu)
           else 1.4826 * medianX(abs(x - mu), weights = weights),
           se = FALSE,
           warn0scale = getOption("verbose"))
  {
    ## Author: Martin Maechler, Date: 6 Jan 2003, ff
    
    ## implicit 'na.rm = TRUE':
    if(any(i <- is.na(x))) {
      x <- x[!i]
      if(!is.null(weights)) weights <- weights[!i]
    }
    n <- length(x)
    sum.w <-
      if(!is.null(weights)) {
        stopifnot(is.numeric(weights), weights >= 0, length(weights) == n)
        sum(weights)
      } else n
    it <- 0L
    NA. <- NA_real_
    if(sum.w == 0) # e.g 'x' was all NA
      return(list(mu = NA., s = NA., it = it, se = NA.)) # instead of error
    
    if (s <= 0) {
      if(s < 0) stop("negative scale 's'")
      if(warn0scale && n > 1)
        warning("scale 's' is zero -- returning initial 'mu'")
    }
    else {
      wsum <- if(is.null(weights)) sum else function(u) sum(u * weights)
      repeat {
        it <- it + 1L
        y <- pmin(pmax(mu - k * s, x), mu + k * s)
        mu1 <- wsum(y) / sum.w
        if (abs(mu - mu1) < tol * s)
          break
        mu <- mu1
      }
    }
    # With weights, as frequency weights: the effective n is sum.w and the
    # sums in tau are weighted, so integer weights reproduce the replicated
    # data exactly. (The original stopped here: "not yet available".)
    list(mu = mu, s = s, it = it,
         SE = if(se) s * sqrt(.tauHuber(x, mu = mu, s = s, k = k,
                                         weights = weights) / sum.w)
              else NA.)
  }



## Originally from  /u/ftp/NDK/Source-NDK-9/R/rg2-fkt.R :
.tauHuber <- function(x, mu, k=1.345, s = mad(x), resid = (x - mu)/s,
                      weights = NULL) {
  ## Purpose: Korrekturfaktor Tau fuer die Varianz von Huber-M-Schaetzern
  ## -------------------------------------------------------------------------
  ## Arguments: x = Daten mu = Lokations-Punkt k = Parameter der Huber Psi-Funktion
  ##            weights = frequency weights (NULL: all 1)
  ## -------------------------------------------------------------------------
  ## Author: Rene Locher Update: R. Frisullo 23.4.02;  M.Maechler (as.log(); s, resid)
  if (is.null(weights)) weights <- rep(1, length(x))
  inr <- abs(resid) <= k
  psi  <- ifelse(inr, resid, sign(resid)*k)                # psi (x)
  psiP <- as.logical(inr)# = ifelse(abs(resid) <= k, 1, 0) # psi'(x)
  sum(weights) * sum(weights * psi^2) / sum(weights * psiP)^2
}
