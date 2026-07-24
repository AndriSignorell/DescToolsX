
#' Tukey's Biweight Mean
#'
#' Computes Tukey's biweight robust mean (also known as the bisquare mean)
#' of a numeric vector, optionally with a bootstrap confidence interval.
#'
#' @details
#' The biweight mean is a robust location estimator that downweights
#' observations far from the median.  It is defined via the tuning constant
#' \code{const} (default 9), which controls the breakdown point: larger
#' values are less resistant but more efficient under normality.
#'
#' When \code{conf.level} is not \code{NA} a bootstrap confidence interval
#' is returned.  Bootstrap arguments are passed through \code{...} and
#' extracted via \code{.extractBootArgs()}:
#' \describe{
#'   \item{\code{R}}{Number of bootstrap replicates (default \code{999}).}
#'   \item{\code{type}}{CI type: \code{"perc"} or \code{"bca"} (default).}
#'   \item{\code{parallel}}{Parallelisation: \code{"no"}, \code{"multicore"},
#'     or \code{"snow"} (default \code{"no"}).}
#'   \item{\code{ncpus}}{Number of CPUs for parallel bootstrap
#'     (default \code{getOption("boot.ncpus", 1L)}).}
#' }
#'
#' @param x a non-empty numeric vector of data values
#' @param conf.level confidence level of the interval. A single numeric
#'   value in \eqn{(0, 1)}, or \code{NA} (default) to return only the
#'   point estimate.
#' @param sides a character string specifying the side of the interval:
#'   \code{"two.sided"} (default), \code{"left"}, or \code{"right"}.
#'   Partial matching is supported.  \code{"left"} sets \code{uci = Inf};
#'   \code{"right"} sets \code{lci = -Inf}.  Ignored when
#'   \code{conf.level = NA}.
#' @param method confidence interval method. Currently only \code{"boot"} is
#' supported.
#' @param const tuning constant passed to \code{tbrm()}. Defaults to \code{9}.
#' @param na.rm logical. Should missing values be removed before computation?
#' Defaults to \code{FALSE}.
#' @param ... further arguments passed to the bootstrap engine when a
#' confidence interval is requested, including \code{R} and \code{type}
#'
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of Tukey's biweight mean}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#'
#' @examples
#' set.seed(1)
#' x <- c(rnorm(50), 10)   # one outlier
#'
#' tukeyBiweight(x)
#' 
#' set.seed(2)             # will yield reproducible intervals
#' tukeyBiweight(x, conf.level = 0.95)
#' tukeyBiweight(x, conf.level = 0.95, type = "perc", R = 499)
#' tukeyBiweight(x, conf.level = 0.95, type = "bca", R = 499)
#'
#'
#' @family location  
#' @concept location  
#' @concept robust-statistics
#'
#'
#' @export
tukeyBiweight <- function(x,
                          conf.level = NA,
                          sides      = c("two.sided", "left", "right"),
                          method     = "boot",
                          const      = 9,
                          na.rm      = FALSE,
                          ...) {
  
  # --- input checks --------------------------------------------------
  if (!is.numeric(x) || length(x) == 0L)
    stop("Argument 'x' must be a non-empty numeric vector.")
  
  if (na.rm)
    x <- x[!is.na(x)]
  
  if (anyNA(x))
    return(NA_real_)
  
  # --- point estimate only -------------------------------------------
  if (is.na(conf.level))
    return(tbrm(x, const))
  
  # --- CI ------------------------------------------------------------
  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
      conf.level <= 0 || conf.level >= 1)
    stop("Argument 'conf.level' must be a single numeric value in (0, 1).")
  
  sides    <- match.arg(sides)
  method   <- match.arg(method)   # only "boot" for now; extensible
  
  conf_adj <- if (sides != "two.sided") 1 - 2 * (1 - conf.level) else conf.level
  alpha    <- 1 - conf_adj
  
  dots      <- list(...)
  boot_args <- .extractBootArgs(dots)
  
  # ------------------------------------------
  # Deterministic base seed from R RNG
  # ------------------------------------------
  base_seed <- as.integer(sample.int(.Machine$integer.max, 1))
  
  
  raw <- tbrm_boot_cpp(
    x,
    R        = boot_args$R,
    alpha    = alpha,
    seed     = base_seed,
    method   = boot_args$type
  )
  
  res <- c(est = raw[["est"]], lci = raw[["lci"]], uci = raw[["uci"]])
  
  if (sides == "left")
    res[["uci"]] <- Inf
  else if (sides == "right")
    res[["lci"]] <- -Inf
  
  res
}
