
#' Safe (Generalized) Huber M-Estimator of Location
#'
#' A (generalized) Huber M-estimator of location with MAD scale that handles
#' the degenerate case of zero scale gracefully, where
#' \code{\link[MASS]{huber}()} would return an error.
#'
#' @details
#' **Wald interval** (\code{method = "wald"})
#'
#' Uses the \eqn{\tau} correction factor of Ruckstuhl following
#' \deqn{
#'   \hat\mu \;\pm\; t_{\alpha/2,\,n-1}
#'   \sqrt{\hat\tau(x,\hat\mu)} \;\frac{\hat s}{\sqrt{n}}
#' }
#' No finite-sample correction is applied beyond the t-quantile.
#'
#' **Bootstrap interval** (\code{method = "boot"})
#'
#' The statistic \eqn{\hat\mu} is resampled \eqn{R} times.  Note that
#' \code{mu} and \code{s} are fixed at their initial values (computed from
#' the full data before resampling) and are not re-estimated on each
#' resample.  The bootstrap therefore targets the variability of the
#' location estimator with fixed scale initialization, not a fully
#' re-estimated robust estimator.  Bootstrap
#' arguments are passed through \code{...} and extracted via
#' \code{.extractBootArgs()}:
#' \describe{
#'   \item{\code{R}}{number of bootstrap replicates (default \code{999})}
#'   \item{\code{type}}{confidence interval type: \code{"perc"} or
#'     \code{"bca"} (default)}
#'   \item{\code{parallel}}{parallelization mode: \code{"no"},
#'     \code{"multicore"}, or \code{"snow"} (default \code{"no"})}
#'   \item{\code{ncpus}}{number of CPUs for parallel bootstrap (default
#'     \code{getOption("boot.ncpus", 1L)})}
#' }
#'
#' The original internal estimator is accessible as
#' \code{DescToolsX:::.huberM}.
#'
#' @param x numeric vector of data values
#' 
#' @param conf.level confidence level of the interval. If set to \code{NA}
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of \code{"two.sided"} (default), \code{"left"} or
#'   \code{"right"}). See \code{\link{ConfidenceIntervals}}.
#'
#' @param method confidence interval method: \code{"wald"} (default) or
#' \code{"boot"}.
#' 
#' @param k positive tuning constant; the algorithm winsorizes at \code{k}
#' standard deviations. Default is \code{1.345}.
#' @param mu initial location estimate. \code{NULL} (default)
#'   uses \code{median(x)}, computed after \code{na.rm} is applied.
#' @param s scale estimate held constant through the iterations.
#'   \code{NULL} (default) uses \code{mad(x, center = mu)}, computed
#'   after \code{na.rm} is applied.
#' @param na.rm logical; whether to remove missing values before computation;
#' default is \code{FALSE}
#' @param ... further arguments passed to the bootstrap engine when
#'   \code{method = "boot"}: \code{R}, \code{type}, \code{parallel},
#'   and \code{ncpus}; see Details
#'
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{location estimate from Huber's M-estimator}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#'
#' @note Adapted from code by Martin Maechler to conform to package standards
#'
#' @references
#' Huber, P. J. (1981). \emph{Robust Statistics}. Wiley.
#'
#' @seealso \code{\link[MASS]{huber}}, \code{\link{mad}},
#'   \code{\link{tukeyBiweight}}
#'
#' @section Random number generation:
#' \code{method = "boot"} resamples and therefore advances R's global
#' random number generator. Call \code{\link[base]{set.seed}} beforehand
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
                   na.rm      = FALSE,
                   ...) {
  
  # --- input checks --------------------------------------------------
  if (!is.numeric(x) || length(x) == 0L)
    stop("Argument 'x' must be a non-empty numeric vector.")
  
  if (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k <= 0)
    stop("Argument 'k' must be a single positive finite number.")
  
  if (na.rm)
    x <- x[!is.na(x)]
  
  if (anyNA(x)) {
    if (length(conf.level) == 1L && is.na(conf.level))
      return(NA_real_)
    return(c(est = NA_real_, lci = NA_real_, uci = NA_real_))
  }
  
  # --- defaults for mu / s after NA removal --------------------------
  if (is.null(mu)) {
    mu <- median(x)
  } else {
    if (!is.numeric(mu) || length(mu) != 1L || !is.finite(mu))
      stop("Argument 'mu' must be a single finite numeric value.")
  }
  
  if (is.null(s)) {
    s <- mad(x, center = mu)
  } else {
    if (!is.numeric(s) || length(s) != 1L || !is.finite(s) || s < 0)
      stop("Argument 's' must be a single non-negative finite numeric value.")
  }
  
  # --- point estimate only -------------------------------------------
  if (is.na(conf.level))
    return(.huberM(x = x, k = k, mu = mu, s = s,
                   warn0scale = TRUE)$mu)
  
  # --- CI ------------------------------------------------------------
  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
      conf.level <= 0 || conf.level >= 1)
    stop("Argument 'conf.level' must be a single numeric value in (0, 1).")
  
  sides  <- match.arg(sides)
  method <- match.arg(method)
  
  conf_adj <- if (sides != "two.sided") 1 - 2 * (1 - conf.level) else conf.level
  alpha    <- 1 - conf_adj
  
  res <- switch(method,
                
                wald = {
                  hm  <- .huberM(x = x, k = k, mu = mu, s = s,
                                 se = TRUE, warn0scale = TRUE)

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
                  ci <- qt(1 - alpha / 2, length(x) - 1L) * hm$SE
                  c(est = hm$mu, lci = hm$mu - ci, uci = hm$mu + ci)
                },
                
                boot = {
                  dots      <- list(...)
                  boot_args <- .extractBootArgs(dots)
                  
                  # freeze mu/s/k - not re-estimated per resample (see @details)
                  k_  <- k
                  mu_ <- mu
                  s_  <- s
                  
                  boot_obj <- boot::boot(
                    x,
                    statistic = function(x, d)
                      .huberM(x = x[d], k = k_, mu = mu_, s = s_,
                              warn0scale = FALSE)$mu,
                    R        = boot_args$R,
                    sim      = "ordinary",
                    parallel = boot_args$parallel,
                    ncpus    = boot_args$ncpus
                  )
                  
                  ci_obj <- boot::boot.ci(boot_obj,
                                          conf = conf_adj,
                                          type = boot_args$type)
                  
                  # boot.ci slot names by type:
                  #   "norm"  -> $normal  (cols: level, lci, uci)
                  #   "perc"  -> $percent (cols: level, ?, ?, lci, uci)
                  #   "bca"   -> $bca     (cols: level, ?, ?, lci, uci)
                  ci_mat <- switch(boot_args$type,
                                   norm  = ci_obj$normal,
                                   perc  = ci_obj$percent,
                                   bca   = ci_obj$bca
                  )
                  
                  if (is.null(ci_mat))
                    stop(
                      "boot.ci() returned NULL for type = '", boot_args$type, "'. ",
                      "Try a different 'type', increase 'R', or use method = 'wald'."
                    )
                  
                  bounds <- if (boot_args$type == "norm")
                    ci_mat[2:3] else ci_mat[4:5]

                  c(est = unname(boot_obj$t0),
                    lci = unname(bounds[1L]),
                    uci = unname(bounds[2L]))
                }
  )
  
  # --- one-sided truncation ------------------------------------------
  if (sides == "left")
    res[["uci"]] <- Inf
  else if (sides == "right")
    res[["lci"]] <- -Inf
  
  res
}




# == internal helper functions ========================================


##  A modified "safe" (and more general) Huber estimator:
.huberM <-
  function(x, k = 1.345, weights = NULL,
           tol = 1e-06,
           mu = if(is.null(weights)) median(x) else .wgt.himedian(x, weights),
           s = if(is.null(weights)) mad(x, center=mu)
           else .wgt.himedian(abs(x - mu), weights),
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
    
    if(se && !is.null(weights))
      stop("Std.error computation not yet available for the case of 'weights'")
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
    list(mu = mu, s = s, it = it,
         SE = if(se) s * sqrt(.tauHuber(x, mu=mu, s=s, k=k) / n) else NA.)
  }



## Originally from  /u/ftp/NDK/Source-NDK-9/R/rg2-fkt.R :
.tauHuber <- function(x, mu, k=1.345, s = mad(x), resid = (x - mu)/s) {
  ## Purpose: Korrekturfaktor Tau fuer die Varianz von Huber-M-Schaetzern
  ## -------------------------------------------------------------------------
  ## Arguments: x = Daten mu = Lokations-Punkt k = Parameter der Huber Psi-Funktion
  ## -------------------------------------------------------------------------
  ## Author: Rene Locher Update: R. Frisullo 23.4.02;  M.Maechler (as.log(); s, resid)
  inr <- abs(resid) <= k
  psi  <- ifelse(inr, resid, sign(resid)*k)                # psi (x)
  psiP <- as.logical(inr)# = ifelse(abs(resid) <= k, 1, 0) # psi'(x)
  length(x) * sum(psi^2) / sum(psiP)^2
}



.wgt.himedian <- function(x, weights = rep(1,n)) {
  
  n <- length(x <- as.double(x))
  stopifnot(storage.mode(weights) %in% c("integer", "double"))
  if(n != length(weights))
    stop("'weights' must have same length as 'x'")
  if(is.integer(weights))
    himed_int_weighted_cpp(x, weights)
  else
    himed_weighted_cpp(x, weights)
}
