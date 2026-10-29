
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
#'   (the default), only the point estimate is returned. One-sided
#'   intervals require `conf.level > 0.5`.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()].
#' @param method interval method: `"exact"` (default), the distribution-free
#'   interval obtained by inverting the Wilcoxon signed-rank (one-sample) or
#'   rank-sum (two-sample) test; or `"boot"`, a bootstrap interval
#'   (one-sample only). Ignored without `conf.level`.
#' @param na.rm logical; if `TRUE`, missing values are removed from `x` and
#'   `y` separately before the computation
#' @param ... additional arguments for `method = "boot"` (`R`, `type`,
#'   `parallel`, `ncpus`); an error with `method = "exact"`.
#'   `type = "stud"` is not available: the bootstrap statistic carries no
#'   variance estimate per resample.
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
#' **Infinite values** are ordinary order statistics. `NaN` is returned
#' when the median is undefined: if a pair yields `Inf - Inf` (`-Inf` and
#' `Inf` both in `x` in the one-sample case, the same infinity in `x` and
#' `y` in the two-sample case), or if the two middle order statistics are
#' `-Inf` and `Inf`.
#'
#' **Exact interval.** With `method = "exact"` the interval inverts the
#' Wilcoxon signed-rank (one-sample) resp. rank-sum (two-sample) test
#' (Bauer 1972). Its bounds are Walsh averages resp. differences
#' `x[i] - y[j]`, selected by Monahan's partitioning instead of sorting all
#' \eqn{O(n^2)} values, so large samples are no problem.
#' \itemize{
#'   \item Samples below 50 (both below 50 in the two-sample case) use
#'     the exact null distribution: the classic one without ties, the
#'     conditional (permutation) distribution given the midranks with ties.
#'   \item Larger samples use the normal approximation with continuity and
#'     tie correction.
#' }
#' The results agree with `wilcox.test(exact = TRUE, conf.int = TRUE)`
#' (R >= 4.6.0) in the two-sample case and in the one-sample case without
#' ties. With ties, `wilcox.test()` (R 4.6.1) takes the lower one-sample
#' bound from \eqn{P(V > v)} instead of \eqn{P(V \ge v)}, one rank too
#' narrow, with coverage below the nominal level; `hodgesLehmann()` uses
#' \eqn{P(V \ge v)}. In the large-sample case `wilcox.test()` interpolates
#' between Walsh averages with [uniroot()], `hodgesLehmann()` returns the
#' order statistics that bound the inverted test.
#' If the requested level is not attainable (very small samples, e.g.
#' \eqn{n \le 5} at 95\%), the bound is infinite and a warning is issued.
#'
#' **Bootstrap interval.** If all bootstrap replicates coincide (e.g.
#' constant data), the interval collapses to `lci = uci = est`. With a
#' single observation, or when the replicates are not all finite, no
#' interval can be formed: `lci` and `uci` are `NA` with a warning.
#'
#' `x` and `y` are not modified.
#'
#' @section Random number generation:
#' Only `method = "boot"` uses R's random number generator; call
#' [base::set.seed()] beforehand for reproducible intervals. The point
#' estimate and the exact interval are deterministic: the compiled routine
#' picks its pivots from a local generator and does not touch R's stream.
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
#' Bauer, D. F. (1972). Constructing confidence sets using rank
#' statistics. *Journal of the American Statistical Association*, 67(339),
#' 687–690. \doi{10.1080/01621459.1972.10481279}
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
#' # distribution-free interval, one- and two-sample
#' hodgesLehmann(x, conf.level = 0.95)
#' hodgesLehmann(x, y, conf.level = 0.95)
#' hodgesLehmann(x, conf.level = 0.95, sides = "left")
#'
#' # bootstrap interval (one-sample)
#' set.seed(1)
#' hodgesLehmann(x, conf.level = 0.95, method = "boot", R = 999)
#'
#' @family location
#' @concept location
#' @concept robust-statistics
#' @export
hodgesLehmann <- function(x,
                          y = NULL,
                          conf.level = NA,
                          sides = c("two.sided", "left", "right"),
                          method = c("exact", "boot"),
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

  checkFlag(na.rm)

  # only NA switches the interval off; NaN is an invalid level and goes
  # to checkConfLevel()
  withCI <- !(length(conf.level) == 1L && is.na(conf.level) &&
                !is.nan(conf.level))

  if (withCI) {

    checkConfLevel(conf.level)

    sides  <- match.arg(sides)
    method <- match.arg(method)

    # one-sided at a level <= 0.5 is meaningless, and for the bootstrap
    # 1 - 2 * (1 - conf.level) would not even stay positive
    if (sides != "two.sided" && conf.level <= 0.5)
      stop("one-sided intervals require 'conf.level' > 0.5")

    if (method == "exact") {

      if (...length() > 0L)
        stop("'...' is only used with method = \"boot\"")

    } else {

      if (!is.null(y))
        stop("bootstrap intervals are implemented only for the one-sample ",
             "case; use method = \"exact\"")

      # validated here as well, so that a bad R or type is reported even
      # when the data turn out to be missing
      args <- .extractBootArgs(list(...))

      if (identical(args$type, "stud"))
        stop("type = \"stud\" is not available: the bootstrap statistic ",
             "provides no variance estimate per resample")
    }
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

  if (method == "exact")
    .hodgesLehmann.exact(x, y, conf.level = conf.level, sides = sides)
  else
    .hodgesLehmann.boot(x, conf.level = conf.level, sides = sides, args = args)
}




# == internal helper functions ================================================

# == exact (rank-based) interval ===============================================
#
# The interval inverts the signed-rank (one-sample) resp. rank-sum
# (two-sample) test. Its bounds are Walsh averages resp. differences
# d = x[i] - y[j], since the test statistic only changes where mu crosses
# one of them. Everything is expressed through the LOWER bound; the upper
# bound is the mirrored lower bound of the negated data.
#
# Between two consecutive distinct values of d, ties in |x - mu| (resp. in
# c(x - mu, y)) can only stem from equal values within x (resp. within x or
# within y): |x[i] - mu| = |x[j] - mu| for x[i] != x[j] means that mu is a
# Walsh average, x[i] - mu = y[j] that mu is a difference. Hence
#   * without such ties the null distribution is the classic one, and the
#     bound is an order statistic at a closed-form rank;
#   * the normal approximation has a constant tie-corrected variance there,
#     so also yields a closed-form rank;
#   * only the exact conditional distribution depends on the
#     arrangement of the tie groups, i.e. on mu, and needs a search.
# The lower bound is the first d(k) beyond which the upper tail
# P(T >= t) exceeds alpha, as in wilcox.test() (R >= 4.6.0) - except that
# its one-sample conditional branch uses P(V > v) there (off by one).

.hlTol <- 10 * .Machine$double.eps


# Lower confidence bound for the one-sided error probability alpha; carries
# attr "unattainable" = TRUE if alpha cannot be met (bound -Inf).
.hlLower <- function(alpha, x, y = NULL) {

  one   <- is.null(y)
  n     <- as.numeric(length(x))   # double: n * m * (n + m + 1) overflows int
  m     <- if (one) 0 else as.numeric(length(y))
  exact <- n < 50 && m < 50
  ties  <- anyDuplicated(x) || (!one && anyDuplicated(y))

  if (exact && ties)
    return(.hlLowerCond(alpha, x, y))

  N <- if (one) n * (n + 1) / 2 else n * m

  C <- if (exact) {
    p <- if (one) function(q) psignrank(q, n) else function(q) pwilcox(q, n, m)
    q <- if (one) qsignrank(alpha, n) else qwilcox(alpha, n, m)
    # q is the smallest with P(W <= q) >= alpha; at equality q + 1 is
    # still valid
    q + (p(q) <= alpha + .hlTol)

  } else {
    # normal approximation with continuity correction and tie-corrected
    # variance: smallest q with pnorm((q + 0.5 - N/2) / s) >= alpha
    tt <- function(v) { t <- table(v); sum(t^3 - t) }
    s2 <- if (one)
      n * (n + 1) * (2 * n + 1) / 24 - tt(x) / 48
    else
      n * m / 12 * ((n + m + 1) - (tt(x) + tt(y)) / ((n + m) * (n + m - 1)))
    max(0, ceiling(N / 2 - 0.5 + qnorm(alpha) * sqrt(s2)))
  }

  if (C == 0)
    return(structure(-Inf, unattainable = TRUE))

  if (one) hlqorder_cpp(x, C) else hl2qorder_cpp(x, y, C)
}


# Exact conditional lower bound (small samples with ties). u are the
# distinct values of d; interval j = 0..J is (u[j], u[j+1]) with
# u[0] = -Inf, u[J+1] = Inf. The upper tail at interval j increases with j,
# so the first j exceeding alpha is found by bisection - O(log J) instead
# of the linear scan in wilcox.test().
.hlLowerCond <- function(alpha, x, y = NULL) {

  one <- is.null(y)
  d <- if (one) {
    w <- outer(x, x, "+") / 2
    w[upper.tri(w, diag = TRUE)]
  } else
    outer(x, y, "-")
  u <- sort(unique(as.vector(d)))
  J <- length(u)

  # does the upper tail on interval j exceed alpha?
  exceeds <- function(j) {
    if (j == J) return(TRUE)                        # statistic at its minimum
    lo <- if (j == 0) -Inf else u[j]
    hi <- u[j + 1L]
    if (lo == hi) return(FALSE)                     # empty: u[1] = -Inf
    mu <- if (is.finite(lo) && is.finite(hi)) (lo + hi) / 2 else
      if (is.finite(hi)) hi - 1 else lo + 1

    p <- if (one) {
      z <- rank(abs(x - mu))
      signrankUpper_cpp(as.integer(2 * z), 2 * sum(z[x > mu]))
    } else {
      z <- rank(c(x - mu, y))
      wilcoxUpper_cpp(as.integer(2 * z), length(x), 2 * sum(z[seq_along(x)]))
    }
    p > alpha + .hlTol
  }

  lo <- 0L; hi <- J
  while (lo < hi) {
    mid <- (lo + hi) %/% 2L
    if (exceeds(mid)) hi <- mid else lo <- mid + 1L
  }

  if (lo == 0L) structure(-Inf, unattainable = TRUE) else u[lo]
}


# x, y: numeric, no NA, length >= 1; conf.level, sides already validated
.hodgesLehmann.exact <- function(x, y, conf.level, sides) {

  alpha <- 1 - conf.level
  if (sides == "two.sided") alpha <- alpha / 2

  neg <- function(v) if (is.null(v)) NULL else -v

  est <- if (is.null(y)) hlqest_cpp(x) else hl2qest_cpp(x, y)
  lci <- if (sides == "right") -Inf else .hlLower(alpha, x, y)
  uci <- if (sides == "left")   Inf else -.hlLower(alpha, -x, neg(y))

  if (isTRUE(attr(lci, "unattainable")) || isTRUE(attr(uci, "unattainable")))
    warning("requested 'conf.level' not attainable with this sample size; ",
            "the bound is infinite", call. = FALSE)

  c(est = est, lci = as.vector(lci), uci = as.vector(uci))
}


# x: numeric, no NA, length >= 1; conf.level, sides and args already
# validated by the caller
.hodgesLehmann.boot <- function(x, conf.level, sides, args) {

  if (sides != "two.sided")
    conf.level <- 1 - 2 * (1 - conf.level)

  est <- hlqest_cpp(x)

  # no interval: named NA bounds plus warning, the documented shape stays
  noCI <- function(reason) {
    warning("no confidence interval: ", reason, call. = FALSE)
    c(est = est, lci = NA_real_, uci = NA_real_)
  }

  if (length(x) < 2L)
    return(noCI("a single observation has no sampling variability"))

  boot.fun <- boot::boot(

    x,

    function(x, d)
      hlqest_cpp(x[d]),

    R        = args$R,
    parallel = args$parallel,
    ncpus    = args$ncpus
  )

  tt <- boot.fun$t[, 1L]

  if (!all(is.finite(tt)))
    return(noCI("bootstrap replicates are not all finite"))

  if (all(tt == est)) {
    # point mass: every interval type collapses onto the estimate, while
    # boot.ci() itself would return NULL
    bounds <- c(est, est)

  } else {

    ci <- boot::boot.ci(boot.fun, conf = conf.level, type = args$type)

    # by name, not by position: ci[[4]] happens to be the first interval
    # component only because exactly one type is requested
    ciMat <- ci[[switch(args$type,
                        norm = "normal", basic = "basic",
                        perc = "percent", bca = "bca")]]

    if (is.null(ciMat))
      return(noCI("boot.ci() returned no interval"))

    bounds <- if (args$type == "norm") ciMat[2:3] else ciMat[4:5]
  }

  res <- c(
    est = est,
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
