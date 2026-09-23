
#' Confidence Intervals for Median Absolute Deviations
#'
#' @description
#' Confidence intervals for the median absolute deviation (MAD) of a single
#' sample (`madCI`), for the difference of two MADs (`madDiffCI`),
#' and for the squared ratio of two MADs (`madRatioCI`).  Two methods
#' are available throughout: an asymptotic interval based on the generalized
#' lambda distribution (GLD), and a parallel bootstrap interval.
#'
#' @details
#' **Classic method** (`method = "classic"`)
#'
#' All three functions follow Arachchige & Prendergast (2019) and base the
#' interval on the asymptotic variance of the MAD, approximated by fitting
#' a GLD to the data via `.asv.mad()`.  The GLD estimation method is
#' selected with `gldMethod`.
#'
#' For `madDiffCI` the asymptotic variances of the two samples are
#' combined as
#' \eqn{\widehat{\mathrm{ASV}}(x)/n_x + \widehat{\mathrm{ASV}}(y)/n_y}.
#'
#' For `madRatioCI` the interval is constructed on the log scale via
#' the delta method and back-transformed to guarantee positivity:
#' \deqn{
#'   \exp\!\Bigl(\log\hat\theta \;\pm\; z_{\alpha/2}
#'   \,\sqrt{\widehat{\mathrm{Var}}(\hat\theta)}\,/\,\hat\theta\Bigr),
#'   \quad \hat\theta = \bigl(\mathrm{MAD}(x)/\mathrm{MAD}(y)\bigr)^2.
#' }
#'
#' The classic method is fast and accurate for large samples but may
#' undercover for small or heavy-tailed distributions.
#'
#' **Bootstrap method** (`method = "boot"`)
#'
#' Data are resampled \eqn{R} times using a parallel Rcpp worker and a
#' percentile or BCa interval is returned.  For the two-sample functions
#' the two samples are resampled independently.  Bootstrap arguments are
#' passed through `...` and extracted via `.extractBootArgs()`:
#' \describe{
#'   \item{`R`}{Number of bootstrap replicates (default `999`).}
#'   \item{`type`}{CI type: `"perc"` or `"bca"` (default).}
#'   \item{`parallel`}{Parallelisation: `"no"`, `"multicore"`,
#'     or `"snow"` (default `"no"`).}
#'   \item{`ncpus`}{Number of CPUs for parallel bootstrap
#'     (default `getOption("boot.ncpus", 1L)`).}
#' }
#'
#' @param x        A non-empty numeric vector (first or only sample).
#' @param y        A non-empty numeric vector (second sample).
#'   Required for `madDiffCI` and `madRatioCI`.
#' @param conf.level Confidence level of the interval.  A single numeric
#'   value in \eqn{(0, 1)}.  Default `0.95`.
#' @param sides    A character string specifying the side of the interval:
#'   `"two.sided"` (default), `"left"`, or `"right"`.
#'   Partial matching is supported.  `"left"` sets `uci = Inf`;
#'   `"right"` sets `lci = -Inf`.
#' @param method   A character string selecting the CI method:
#'   `"classic"` (asymptotic GLD-based, default) or `"boot"`
#'   (parallel bootstrap).
#' @param gldMethod A character string passed to `.asv.mad()` selecting
#'   the GLD estimation method.  One of `"ML"`, `"MPS"`,
#'   `"TM"` (default), `"SM"`, `"TL"`, `"Lmom"`,
#'   `"DLA"`, or `"Mom"`.  See [gld::fit.fkml()].
#'   Used only when `method = "classic"`.
#' @param na.rm    Logical.  Should missing values be removed before
#'   computation?  Default `FALSE`.
#' @param ...      Further arguments passed to the bootstrap engine when
#'   `method = "boot"`: `R`, `type`, `parallel`,
#'   `ncpus`.  See Details.
#'
#' 
#' @return A named numeric vector with three elements:
#' \itemize{
#'   \item `est`: point estimate:\cr
#'        \verb{  } \eqn{\mathrm{mad}(x)} for `madCI`\cr
#'        \verb{  } \eqn{\mathrm{mad}(x) - \mathrm{mad}(y)}
#'           for `madDiffCI` \cr
#'        \verb{  } \eqn{(\mathrm{mad}(x)/\mathrm{mad}(y))^2}
#'           for `madRatioCI`
#'   \item `lci`: lower confidence bound.
#'   \item `uci`: upper confidence bound.
#' }
#'    
#' @note
#' Based on code by Arachchige Chandima N. P. G. and Prendergast Luke A.,
#' adapted to conform to package standards.
#'
#' @references
#' Arachchige, C. N. P. G., & Prendergast, L. A. (2019). Confidence
#'   intervals for median absolute deviations. *arXiv:1910.00229*
#'   \verb{[math.ST]}.
#'
#' @seealso [mad()], `DescToolsX::madX`
#'
#' @examples
#' set.seed(1)
#' x <- rlnorm(100)
#' y <- rlnorm(200, meanlog = 1.2)
#'
#' # single sample
#' madCI(x)
#' madCI(x, sides = "left")
#' madCI(x, method = "boot", R = 499, type = "bca")
#'
#' # two-sample difference
#' madDiffCI(x, y)
#' madDiffCI(x, y, method = "boot", R = 499, type = "perc")
#'
#' # two-sample squared ratio
#' madRatioCI(x, y)
#' madRatioCI(x, y, method = "boot", R = 499, type = "bca")
#'
#' @name mad-confints
#' @aliases madCI madDiffCI madRatioCI
#' 
#' @family ci.variance  
#' @concept confidence-interval  
#' @concept robust-statistics
NULL


#' @rdname mad-confints
#' @export
madCI <- function(x,
                  conf.level = 0.95,
                  sides      = c("two.sided", "left", "right"),
                  method     = c("classic", "boot"),
                  gldMethod  = "TM",
                  na.rm      = FALSE,
                  ...) {
  
  # --- input checks --------------------------------------------------
  if (!is.numeric(x) || length(x) == 0L)
    stop("Argument 'x' must be a non-empty numeric vector.")
  
  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
      conf.level <= 0 || conf.level >= 1)
    stop("Argument 'conf.level' must be a single numeric value in (0, 1).")
  
  if (na.rm)
    x <- x[!is.na(x)]
  
  if (length(x) == 0L)
    stop("No non-missing values in 'x' after applying 'na.rm'.")
  
  sides  <- match.arg(sides)
  method <- match.arg(method)
  
  # one-sided: adjust conf.level to two-sided equivalent for computation
  conf_adj <- if (sides != "two.sided") 1 - 2 * (1 - conf.level) else conf.level
  alpha    <- 1 - conf_adj
  
  # --- estimate ------------------------------------------------------
  est <- mad(x)
  
  # --- CI ------------------------------------------------------------
  res <- switch(method,
                
                classic = {
                  
                  z   <- qnorm(1 - alpha / 2)
                  n   <- length(x)
                  asv <- .asv.mad(x, method = gldMethod)
                  ci  <- est + c(-z, z) * sqrt(asv / n)
                  c(est = est, lci = ci[1L], uci = ci[2L])
                },
                
                boot = {
                  
                  dots      <- list(...)
                  boot_args <- .extractBootArgs(dots)
                  
                  raw <- mad_boot_cpp(
                    x,
                    R      = boot_args$R,
                    alpha  = alpha,
                    seed   = -1L,
                    method = boot_args$type          # "perc" or "bca"
                  )
                  
                  c(est = raw[["est"]], lci = raw[["lci"]], uci = raw[["uci"]])
                }
  )
  
  # --- one-sided truncation ------------------------------------------
  if (sides == "left")
    res[["uci"]] <- Inf
  else if (sides == "right")
    res[["lci"]] <- -Inf
  
  res
}




#' @rdname mad-confints
#' @export
madDiffCI <- function(x, y,
                      conf.level = 0.95,
                      sides      = c("two.sided", "left", "right"),
                      method     = c("classic", "boot"),
                      gldMethod  = "TM",
                      na.rm      = FALSE,
                      ...) {
  
  # --- input checks --------------------------------------------------
  if (!is.numeric(x) || length(x) == 0L)
    stop("Argument 'x' must be a non-empty numeric vector.")
  
  if (!is.numeric(y) || length(y) == 0L)
    stop("Argument 'y' must be a non-empty numeric vector.")
  
  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
      conf.level <= 0 || conf.level >= 1)
    stop("Argument 'conf.level' must be a single numeric value in (0, 1).")
  
  if (na.rm) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }
  
  if (length(x) == 0L)
    stop("No non-missing values in 'x' after applying 'na.rm'.")
  
  if (length(y) == 0L)
    stop("No non-missing values in 'y' after applying 'na.rm'.")
  
  sides  <- match.arg(sides)
  method <- match.arg(method)
  
  conf_adj <- if (sides != "two.sided") 1 - 2 * (1 - conf.level) else conf.level
  alpha    <- 1 - conf_adj
  
  # --- estimate ------------------------------------------------------
  est <- mad(x) - mad(y)
  
  # --- CI ------------------------------------------------------------
  res <- switch(method,
                
                classic = {
                  
                  z     <- qnorm(1 - alpha / 2)
                  asv.x <- .asv.mad(x, method = gldMethod)
                  asv.y <- .asv.mad(y, method = gldMethod)
                  ci    <- est + c(-z, z) * sqrt(asv.x / length(x) + asv.y / length(y))
                  c(est = est, lci = ci[1L], uci = ci[2L])
                },
                
                boot = {
                  
                  dots      <- list(...)
                  boot_args <- .extractBootArgs(dots)
                  
                  raw <- mad_diff_boot_cpp(
                    x,
                    y,
                    R      = boot_args$R,
                    alpha  = alpha,
                    seed   = -1L,
                    method = boot_args$type
                  )
                  
                  c(est = raw[["est"]], lci = raw[["lci"]], uci = raw[["uci"]])
                }
  )
  
  # --- one-sided truncation ------------------------------------------
  if (sides == "left")
    res[["uci"]] <- Inf
  else if (sides == "right")
    res[["lci"]] <- -Inf
  
  res
}




#' @rdname mad-confints
#' @export
madRatioCI <- function(x, y,
                       conf.level = 0.95,
                       sides      = c("two.sided", "left", "right"),
                       method     = c("classic", "boot"),
                       gldMethod  = "TM",
                       na.rm      = FALSE,
                       ...) {
  
  # --- input checks --------------------------------------------------
  if (!is.numeric(x) || length(x) == 0L)
    stop("Argument 'x' must be a non-empty numeric vector.")
  
  if (!is.numeric(y) || length(y) == 0L)
    stop("Argument 'y' must be a non-empty numeric vector.")
  
  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
      conf.level <= 0 || conf.level >= 1)
    stop("Argument 'conf.level' must be a single numeric value in (0, 1).")
  
  if (na.rm) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }
  
  if (length(x) == 0L)
    stop("No non-missing values in 'x' after applying 'na.rm'.")
  
  if (length(y) == 0L)
    stop("No non-missing values in 'y' after applying 'na.rm'.")
  
  sides  <- match.arg(sides)
  method <- match.arg(method)
  
  conf_adj <- if (sides != "two.sided") 1 - 2 * (1 - conf.level) else conf.level
  alpha    <- 1 - conf_adj
  
  # --- estimate ------------------------------------------------------
  mad.x <- mad(x)
  mad.y <- mad(y)
  
  if (mad.y == 0)
    stop("MAD of 'y' is zero; ratio is undefined.")
  
  est <- (mad.x / mad.y)^2
  
  # --- CI ------------------------------------------------------------
  res <- switch(method,
                
                classic = {
                  
                  z     <- qnorm(1 - alpha / 2)
                  asv.x <- .asv.mad(x, method = gldMethod)
                  asv.y <- .asv.mad(y, method = gldMethod)
                  
                  # delta-method variance of est = (mad.x / mad.y)^2
                  var.est <- 4 * est *
                    ((1 / mad.y^2) * asv.x / length(x) +
                       (est / mad.y^2) * asv.y / length(y))
                  
                  # log-scale interval for positivity, then back-transform
                  ci <- exp(log(est) + c(-z, z) * sqrt((1 / est^2) * var.est))
                  c(est = est, lci = ci[1L], uci = ci[2L])
                },
                
                boot = {
                  
                  dots      <- list(...)
                  boot_args <- .extractBootArgs(dots)
                  
                  raw <- mad_ratio_boot_cpp(
                    x,
                    y,
                    R      = boot_args$R,
                    alpha  = alpha,
                    seed   = -1L,
                    method = boot_args$type
                  )
                  
                  c(est = raw[["est"]], lci = raw[["lci"]], uci = raw[["uci"]])
                }
  )
  
  # --- one-sided truncation ------------------------------------------
  if (sides == "left")
    res[["uci"]] <- Inf
  else if (sides == "right")
    res[["lci"]] <- -Inf
  
  res
}




# internal helper functions ---------------------------------

.asv.mad <- function(x, method = "TM", constant = 1.4826) {

  # Asymptotic variance of constant * MAD (Arachchige & Prendergast 2019).
  # The formula is stated for the raw MAD zeta, defined by
  # F(m + zeta) - F(m - zeta) = 1/2, so the density must be evaluated at
  # m -/+ mad(x, constant = 1). The CI functions use the consistency-scaled
  # mad(x) as estimate, hence the factor constant^2.
  lambda <- gld::fit.fkml(x, method = method)$lambda
  m      <- median(x)
  zeta   <- mad(x, constant = 1)
  fFinv  <- gld::dgl(c(m - zeta, m + zeta, m), lambda1 = lambda)
  FFinv  <- gld::pgl(c(m - zeta, m + zeta), lambda1 = lambda)
  A <- fFinv[1] + fFinv[2]
  C <- fFinv[1] - fFinv[2]
  B <- C^2 + 4 * C * fFinv[3] * (1 - FFinv[2] - FFinv[1])

  constant^2 * (1 + B / fFinv[3]^2) / (4 * A^2)
}
