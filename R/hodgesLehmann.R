
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
#' of \code{\link{wilcox.test}}
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
#' @param conf.level confidence level; use \code{NA} to return only the point
#' estimate. Confidence intervals are currently available only for the
#' one-sample case.
#' @param sides character string specifying the side of the interval:
#' \code{"two.sided"}, \code{"left"}, or \code{"right"}
#' @param method confidence interval method; currently only
#' \code{"boot"} is implemented
#' @param na.rm logical; whether to remove missing values
#' @param ... additional arguments passed to bootstrap procedures
#'
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of the Hodges-Lehmann location}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#'
#' @details
#' \code{sides} names the side on which the finite bound lies:
#' \code{"left"} yields \eqn{[lci, \infty)}, \code{"right"} yields
#' \eqn{(-\infty, uci]}. The estimator is unbounded, so the open side is
#' reported as \eqn{\pm\infty}.
#'
#' \code{x} and \code{y} are not modified.
#'
#' @section Random number generation:
#' A confidence level triggers a bootstrap and therefore advances R's
#' global random number generator. Call \code{\link[base]{set.seed}}
#' beforehand for reproducible intervals. The point estimate itself is
#' deterministic: the compiled routine picks its pivots from a local
#' generator and does not touch R's stream.
#'
#' @seealso \code{\link[stats]{wilcox.test}}, \code{\link{medianX}}
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
                          method = c("boot"),
                          na.rm = FALSE,
                          ...) {
  

  if (na.rm) {
    if (is.null(y)) {
      x <- na.omit(x)
    } else {
      ok <- complete.cases(x, y)
      x <- x[ok]
      y <- y[ok]
    }
  }
  
  if (anyNA(x) || (!is.null(y) && anyNA(y))) {
    if (is.na(conf.level)) {
      return(NA_real_)
    } else {
      return(c(
        est = NA_real_,
        lci = NA_real_,
        uci = NA_real_
      ))
    }
  }
  
  if (!is.numeric(x))
    stop("'x' must be numeric")
  
  if (!is.null(y) && !is.numeric(y))
    stop("'y' must be numeric")

  if (!is.null(y) && !is.na(conf.level))
    stop("confidence intervals are currently implemented only for the one-sample case")
  
  if (length(x) < 1)
    stop("'x' must contain at least one observation")

  # y was never length-checked, so an empty y reached hl2qest_cpp() and was
  # indexed at y[n - 1] with n = 0
  if (!is.null(y) && length(y) < 1)
    stop("'y' must contain at least one observation")
  
  method <- match.arg(method)

  if (is.null(y)) {
    res <- hlqest_cpp(x)
  } else {
    res <- hl2qest_cpp(x, y)
  }
  
  if (is.na(conf.level)) {

    result <- res
    names(result) <- NULL

  } else {

    # match.arg() above already guarantees "boot", so the former else
    # branch - a warning plus c(est, NA, NA) - was unreachable. The
    # distribution-free interval from the Wilcoxon rank statistic is
    # still worth having; it belongs in method = "exact" when it lands,
    # not in dead code behind the only accepted value.
    #
    # ToDo: two-sample confidence intervals
    result <- .hodgesLehmann.boot(
      x,
      conf.level = conf.level,
      sides = sides,
      ...
    )
  }
  
  result
}




# == internal helper functions ================================================

.hodgesLehmann.boot <- function(x,
                                conf.level,
                                sides = c("two.sided", "left", "right"),
                                ...) {
  
  sides <- match.arg(
    sides,
    choices = c("two.sided", "left", "right"),
    several.ok = FALSE
  )
  
  if (sides != "two.sided")
    conf.level <- 1 - 2 * (1 - conf.level)
  
  args <- .extractBootArgs(list(...))
  
  # adjusted bootstrap percentile (BCa) interval
  
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
