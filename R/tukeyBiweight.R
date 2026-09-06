#' Tukey's Biweight Mean
#'
#' Computes Tukey's biweight robust mean (also known as the bisquare mean)
#' of a numeric vector, optionally with a bootstrap confidence interval.
#'
#' @details
#' The biweight mean is a robust location estimator that downweights
#' observations far from the median.  It is defined via the tuning constant
#' `const` (default 9), which controls the breakdown point: larger
#' values are less resistant but more efficient under normality.
#'
#' When `conf.level` is not `NA` a bootstrap confidence interval
#' is returned.  The resampling is done in C++; the R random number generator
#' is used only to draw the seed, so `set.seed()` makes the result
#' reproducible.  Bootstrap arguments are passed through `...`:
#' \describe{
#'   \item{`R`}{Number of bootstrap replicates (default `999`).}
#'   \item{`type`}{CI type: `"perc"` or `"bca"` (default).}
#' }
#'
#' The biweight mean is a location estimator and therefore unbounded, so the
#' open side of a one-sided interval is reported at \eqn{\pm\infty} - unlike
#' the bounded measures in this package, where it is reported at the range
#' limit. See [ConfidenceIntervals()].
#'
#' @param x a non-empty numeric vector of data values
#'
#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()].
#'
#' @param const tuning constant passed to `tbrm_cpp()`. Defaults to `9`.
#' @param na.rm logical. Should missing values be removed before computation?
#' Defaults to `FALSE`.
#' @param ... further arguments passed to the bootstrap engine when a
#' confidence interval is requested, namely `R` and `type`. Any
#' other name is an error rather than a silent no-op.
#'
#' @return if `conf.level = NA`, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{`est`}{point estimate of Tukey's biweight mean}
#'   \item{`lci`}{lower confidence interval bound}
#'   \item{`uci`}{upper confidence interval bound}
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
#' # one-sided: "left" carries the finite lower bound
#' set.seed(2)
#' tukeyBiweight(x, conf.level = 0.95, sides = "left")
#'
#'
#' @family location
#' @concept location
#' @concept robust-statistics
#'
#' @export
tukeyBiweight <- function(x,
                          conf.level = NA,
                          sides      = c("two.sided", "left", "right"),
                          const      = 9,
                          na.rm      = FALSE,
                          ...) {

  # --- argument checks -----------------------------------------------
  # All of them up front, none inside a branch: 'sides' used to be matched
  # in the interval section only, so a misspelled value was accepted
  # whenever conf.level was NA.
  sides <- match.arg(sides)

  conf.level <- checkConfLevel(conf.level)
  checkFlag(na.rm)

  if (!is.numeric(x) || length(x) == 0L)
    stop("Argument 'x' must be a non-empty numeric vector.")

  if (!is.numeric(const) || length(const) != 1L || !is.finite(const) ||
      const <= 0)
    stop("Argument 'const' must be a single positive number.")

  if (sides != "two.sided" && !is.na(conf.level) && conf.level <= 0.5)
    stop(gettextf(
      "a one-sided interval needs 'conf.level' above 0.5, not %g",
      conf.level), domain = NA)

  # 'method' used to be a formal with a single legal value. It was dropped
  # (design_rules.md 4.1: method appears once there are two methods), which
  # would have turned tukeyBiweight(x, method = "asymptotic") from a caught
  # error into a silent no-op - hence the check below. Only R and type are
  # accepted here; there is nothing else to pass anything on to.
  dots <- list(...)
  nms  <- names(dots)
  if (is.null(nms)) nms <- rep("", length(dots))

  bad <- setdiff(nms[nzchar(nms)], .bootArgNames)
  if (length(bad))
    stop(gettextf("unused argument(s) in '...': %s",
                  paste(sQuote(bad, FALSE), collapse = ", ")), domain = NA)

  if (na.rm)
    x <- x[!is.na(x)]

  if (length(x) == 0L)
    stop("No non-missing observations left in 'x'.")

  # --- point estimate only -------------------------------------------
  if (is.na(conf.level)) {
    if (anyNA(x))
      return(NA_real_)
    return(tbrm_cpp(x, C = const))
  }

  # --- confidence interval -------------------------------------------
  # keep the return shape, an NA scalar would break every caller that
  # reads res[["est"]]
  if (anyNA(x))
    return(c(est = NA_real_, lci = NA_real_, uci = NA_real_))

  # A one-sided bound at level gamma is the corresponding end of the
  # two-sided interval at level 2*gamma - 1.
  confAdj <- if (sides == "two.sided") conf.level else 2 * conf.level - 1
  alpha   <- 1 - confAdj

  # perc/bca only: the shared validator offers all five boot.ci types, but
  # tbrm_boot_cpp() computes two of them. 'parallel' and 'ncpus' are boot()
  # arguments and have no effect on the compiled path, so they are refused
  # rather than accepted and ignored.
  #
  # default = "bca" deliberately: the biweight mean is a smooth, unbounded
  # location estimator, which is the side of the family where bca is the
  # better choice.
  boot_args <- .extractBootArgs(dots,
                                types    = c("perc", "bca"),
                                default  = "bca",
                                parallel = FALSE)

  # ------------------------------------------
  # Deterministic base seed from R RNG
  # ------------------------------------------
  base_seed <- as.integer(sample.int(.Machine$integer.max, 1))

  raw <- tbrm_boot_cpp(
    x,
    R        = boot_args$R,
    alpha    = alpha,
    constant = const,          # C++ formal is 'constant', 'const' is a keyword
    seed     = base_seed,
    method   = boot_args$type
  )

  # read by name, not by position: a helper that returns a shorter or
  # differently ordered vector must fail loudly here rather than silently
  # shift the bounds (cf. .relRiskUseOr(), krippAlpha())
  if (!all(c("est", "lci", "uci") %in% names(raw)))
    stop("tbrm_boot_cpp() did not return the expected 'est'/'lci'/'uci' vector.")

  # unbounded location estimator, so the open side really is infinite -
  # applySides() is used all the same, so that every function in the suite
  # opens its interval in one place
  c(est = raw[["est"]],
    applySides(c(raw[["lci"]], raw[["uci"]]), sides, lo = -Inf, hi = Inf))
}
