
#' Hodges-Lehmann Estimator of Location
#'
#' Function to compute the Hodges-Lehmann estimator of location
#' in the one and two sample case following a clever fast algorithm
#' by John Monahan (1984).
#'
#' The Hodges-Lehmann estimator is the median of the combined
#' data points and Walsh averages.
#'
#' It is the same as the pseudo median returned as a by-product
#' of [wilcox.test()]
#' (which however does not calculate correctly as soon as ties
#' are present).
#'
#' Note that in the two-sample case the estimator for the
#' difference in location parameters does not estimate the
#' difference in medians (a common misconception) but rather
#' the median of the difference between a sample from x and
#' a sample from y.
#'
#' @param x numeric vector
#' @param y optional numeric vector
#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()].
#' @param na.rm logical; if `TRUE`, missing values are removed from `x` and
#'   `y` separately before the computation
#' @param ... additional arguments passed to bootstrap procedures
#'
#' @return if `conf.level = NA`, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{`est`}{point estimate of the Hodges-Lehmann location}
#'   \item{`lci`}{lower confidence interval bound}
#'   \item{`uci`}{upper confidence interval bound}
#' }
#'
#' @details
#' `sides` names the side on which the finite bound lies:
#' `"left"` yields \eqn{[lci, \infty)}, `"right"` yields
#' \eqn{(-\infty, uci]}. The estimator is unbounded, so the open side is
#' reported as \eqn{\pm\infty}.
#'
#' **Missing values.** `NA` and `NaN` are treated alike. With
#' `na.rm = FALSE`, a missing value in `x` or `y` yields `NA` - a scalar,
#' or `c(est = NA, lci = NA, uci = NA)` when an interval was requested. With
#' `na.rm = TRUE` the missing values are removed from each sample
#' separately; `x` and `y` are independent samples, not pairs. An empty
#' sample, given or left over after the removal, yields `NA` as well.
#' Invalid arguments are an error even when data are missing.
#'
#' `x` and `y` are not modified.
#'
#' @section Random number generation:
#' A confidence level triggers a bootstrap and therefore advances R's
#' global random number generator. Call [base::set.seed()]
#' beforehand for reproducible intervals. The point estimate itself is
#' deterministic: the compiled routine picks its pivots from a local
#' generator and does not touch R's stream.
#'
#' @note C++ port of Monahan's algorithm by Cyril Flurin Moser
#'
#' @references
#' Hodges, J. L., Lehmann, E. L. (1963). Estimates of location based on
#' rank tests. *The Annals of Mathematical Statistics*, 34(2), 598–611.
#' \doi{10.1214/aoms/1177704172}
#'
#' Monahan, J. F. (1984). Algorithm 616: Fast computation of the
#' Hodges-Lehmann location estimator. *ACM Transactions on Mathematical
#' Software*, 10(3), 265–270. \doi{10.1145/1271.319414}.
#' Original code: \url{https://www4.stat.ncsu.edu/~monahan/jul10/}
#'
#' Efron, B. (1987). Better bootstrap confidence intervals.
#' *Journal of the American Statistical Association*, 82(397), 171–185.
#' \doi{10.1080/01621459.1987.10478410}
#'
#' Davison, A. C., Hinkley, D. V. (1997). *Bootstrap Methods and Their
#' Application*. Cambridge University Press.
#'
#' @seealso [stats::wilcox.test()]
#'
#' @examples
#' x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
#' hodgesLehmann(x)
#'
#' # the input is left alone
#' v <- c(3, 1, 2)
#' hodgesLehmann(v)
#' v
#'
#' # two-sample: median of the pairwise differences, NOT the difference
#' # of the medians
#' y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
#' hodgesLehmann(x, y)
#'
#' # missing values: NA, or removed per sample with na.rm
#' hodgesLehmann(c(x, NA), y)
#' hodgesLehmann(c(x, NA), y[-1], na.rm = TRUE)
#'
#' set.seed(1)
#' hodgesLehmann(x, conf.level = 0.95)
#'
#' @family location
#' @concept location
#' @concept robust-statistics
#' @export
hodgesLehmann <- function(x,
                          y = NULL,
                          conf.level = NA,
                          sides = c("two.sided", "left", "right"),
                          na.rm = FALSE,
                          ...) {

  # == argument checks =======================================================
  #
  # All of them BEFORE any early NA return: missing data must not mask an
  # invalid call.

  # c(NA, NA) is LOGICAL: "all missing" must reach the NA handling below,
  # not fail the type check
  if (is.logical(x) && all(is.na(x))) x <- as.numeric(x)
  if (is.logical(y) && all(is.na(y))) y <- as.numeric(y)

  if (!is.numeric(x))
    stop("'x' must be numeric")

  if (!is.null(y) && !is.numeric(y))
    stop("'y' must be numeric")

  withCI <- !(length(conf.level) == 1L && is.na(conf.level))

  if (withCI) {

    checkConfLevel(conf.level)

    if (!is.null(y))
      stop("confidence intervals are currently implemented only for the one-sample case")

    sides <- match.arg(sides)

    # validated here as well, so that a bad R or type is reported even when
    # the data turn out to be missing
    args <- .extractBootArgs(list(...))
  }


  # == missing values ========================================================
  #
  # Suite rule: na.rm = FALSE -> NA of the documented shape (as mean()),
  # na.rm = TRUE -> remove first. x and y are INDEPENDENT samples, so each
  # is cleaned on its own. The former complete.cases(x, y) paired them:
  # with unequal lengths it stopped, with equal lengths an NA in x silently
  # dropped the valid y at the same position.

  naResult <- if (withCI)
    c(est = NA_real_, lci = NA_real_, uci = NA_real_)
  else
    NA_real_

  if (na.rm) {
    x <- x[!is.na(x)]
    if (!is.null(y)) y <- y[!is.na(y)]

  } else if (anyNA(x) || anyNA(y)) {
    return(naResult)
  }

  # an empty sample - given or left over - is empty data, not a calling
  # error
  if (length(x) == 0L || (!is.null(y) && length(y) == 0L))
    return(naResult)


  # == estimate ==============================================================

  if (!withCI) {
    res <- if (is.null(y)) hlqest_cpp(x) else hl2qest_cpp(x, y)
    return(unname(res))
  }

  # The distribution-free interval from the Wilcoxon rank statistic is
  # still worth having; it belongs in method = "exact" when it lands.
  #
  # ToDo: two-sample confidence intervals
  .hodgesLehmann.boot(x, conf.level = conf.level, sides = sides, args = args)
}




# == internal helper functions ================================================

# x: numeric, no NA, length >= 1; conf.level, sides and args already
# validated by the caller
.hodgesLehmann.boot <- function(x, conf.level, sides, args) {

  if (sides != "two.sided")
    conf.level <- 1 - 2 * (1 - conf.level)

  boot.fun <- boot::boot(

    x,

    function(x, d)
      hlqest_cpp(x[d]),

    R        = args$R,
    parallel = args$parallel,
    ncpus    = args$ncpus
  )

  ci <- boot::boot.ci(
    boot.fun,
    conf = conf.level,
    type = args$type
  )

  # by name, not by position: ci[[4]] happens to be the first interval
  # component only because exactly one type is requested
  ciMat <- ci[[switch(args$type,
                      norm = "normal", basic = "basic", stud = "student",
                      perc = "percent", bca = "bca")]]

  bounds <- if (args$type == "norm") ciMat[2:3] else ciMat[4:5]

  res <- c(
    est = unname(boot.fun$t0),
    lci = unname(bounds[1L]),
    uci = unname(bounds[2L])
  )

  # sides names the side carrying the FINITE bound; the estimator is
  # unbounded, so the open side really is infinite here
  if (sides == "left")
    res[["uci"]] <- Inf
  else if (sides == "right")
    res[["lci"]] <- -Inf

  res
}
