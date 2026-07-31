
#' Glass' Delta Effect Size
#'
#' Computes Glass' delta, a standardized mean difference that uses the
#' standard deviation of the control group only, along with a noncentral-t
#' based confidence interval and an optional small-sample bias correction.
#'
#' @param x numeric vector containing the treatment group
#' @param y numeric vector containing the control group
#' @param conf.level confidence level of the interval. If \code{NA}
#'   (default), only the point estimate is returned.
#' @param sides a character string specifying the side of the confidence
#'   interval, must be one of \code{"two.sided"} (default), \code{"left"}
#'   or \code{"right"}. \code{sides} names the side on which the finite
#'   bound lies: \code{"left"} yields \eqn{[lci, \infty)} and
#'   \code{"right"} \eqn{(-\infty, uci]}. Note that this is the reverse of
#'   the convention in \pkg{DescTools}, where \code{sides} follows the
#'   alternative hypothesis of \code{\link[stats]{t.test}}. You can specify
#'   just the initial letter.
#' @param useControlSd logical, if \code{TRUE} (default) the standard
#'   deviation of the control group \code{y} is used for standardization,
#'   otherwise the one of \code{x}
#' @param correct logical, if \code{TRUE} the exact small-sample bias
#'   correction (Hedges' correction with \eqn{df = n_C - 1}) is applied.
#'   Requires at least 3 observations in the standardizing group.
#'   Default is \code{FALSE}.
#' @param na.rm logical, should missing values be removed? Default is
#'   \code{FALSE}. If \code{FALSE} and any of the groups contains missing
#'   values, \code{NA} is returned.
#'
#' @details
#' Glass' delta is defined as:
#' \deqn{ \Delta = \frac{\bar{x} - \bar{y}}{s_y} }
#' where \eqn{s_y} is the standard deviation of the control group. It is
#' preferred over Cohen's d when the treatment is expected to affect the
#' variance, so that the control group's variability is the natural
#' reference scale.
#'
#' The confidence interval is obtained by inverting the noncentral
#' t-distribution with \eqn{df = n_C - 1} degrees of freedom, where
#' \eqn{n_C} is the size of the group supplying the standard deviation
#' (Kelley, 2007). Note that this interval assumes equal population
#' variances in both groups. Since Glass' delta is typically chosen
#' precisely when the variances are expected to differ, the interval
#' should be regarded as approximate under heteroscedasticity.
#'
#' With \code{correct = TRUE} the exact correction factor
#' \deqn{ J(df) = \frac{\Gamma(df/2)}{\sqrt{df/2}\,\Gamma((df-1)/2)} }
#' is applied to the estimate and both confidence limits.
#'
#' @return a named numeric vector. If \code{conf.level = NA}, only
#' \code{est} is returned; otherwise the vector has elements:
#' \describe{
#'   \item{\code{est}}{point estimate of Glass' delta}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#'
#' In both cases the result carries the attribute \code{"magnitude"} with
#' the conventional interpretation of the estimate's absolute size
#' (\code{"negligible"} < 0.2 \eqn{\le} \code{"small"} < 0.5 \eqn{\le}
#' \code{"medium"} < 0.8 \eqn{\le} \code{"large"}), analogous to
#' \code{cohenD()}.
#'
#' @note The confidence interval method follows Ken Kelley's approach
#' previously published in the \pkg{MBESS} package, reimplemented to
#' conform to package standards.
#'
#' @references
#' Glass, G. V. (1976) Primary, secondary, and meta-analysis of research.
#' \emph{Educational Researcher}, 5(10), 3-8.
#'
#' Hedges, L. V., Olkin, I. (1985) \emph{Statistical Methods for
#' Meta-Analysis}. Orlando: Academic Press.
#'
#' Kelley, K. (2007) Confidence intervals for standardized effect sizes:
#' Theory, application, and implementation. \emph{Journal of Statistical
#' Software}, 20(8), 1-24.
#'
#'
#' @examples
#' set.seed(5)
#' x <- rnorm(30, mean = 1)
#' y <- rnorm(30, mean = 0)
#'
#' glassDelta(x, y)
#'
#' glassDelta(x, y, conf.level = 0.95)
#'
#' # one-sided: "right" bounds the interval from ABOVE
#' glassDelta(x, y, conf.level = 0.95, sides = "right")
#'
#' # ... and "left" from below
#' glassDelta(x, y, conf.level = 0.95, sides = "left")
#'
#' # small-sample bias correction
#' glassDelta(x, y, conf.level = 0.95, correct = TRUE)
#'
#' # standardize by the treatment group instead
#' glassDelta(x, y, useControlSd = FALSE)
#'
#'
#' @seealso \code{\link{cohenD}}
#'
#' @family effect.size
#' @concept effect-size
#'
#' @export

glassDelta <- function(x, y, conf.level = NA,
                       sides = c("two.sided", "left", "right"),
                       useControlSd = TRUE, correct = FALSE, na.rm = FALSE) {

  # -- validate ------------------------------------------------------------
  if (!is.numeric(x) || !is.null(dim(x)))
    stop("'x' must be a numeric vector")

  if (!is.numeric(y) || !is.null(dim(y)))
    stop("'y' must be a numeric vector")

  .checkFlag <- function(a) {
    if (!is.logical(a) || length(a) != 1L || is.na(a))
      stop(gettextf("'%s' must be a single non-missing logical value",
                    deparse1(substitute(a))), call. = FALSE)
  }
  .checkFlag(useControlSd)
  .checkFlag(correct)
  .checkFlag(na.rm)

  if (length(conf.level) != 1L ||
      !((is.logical(conf.level) && is.na(conf.level)) ||
        (is.numeric(conf.level) && !is.nan(conf.level))))
    stop("'conf.level' must be NA or a single numeric value")

  if (!is.na(conf.level) && (conf.level <= 0 || conf.level >= 1))
    stop("'conf.level' must lie strictly between 0 and 1")

  sides <- match.arg(sides)

  # -- missing values -------------------------------------------------------
  if (na.rm) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]

  } else if (anyNA(x) || anyNA(y)) {
    res <- if (is.na(conf.level))
             c(est = NA_real_)
           else
             c(est = NA_real_, lci = NA_real_, uci = NA_real_)
    attr(res, "magnitude") <- NA_character_
    return(res)
  }

  if (any(is.infinite(x)) || any(is.infinite(y)))
    stop("'x' and 'y' must not contain infinite values")

  # -- estimate ------------------------------------------------------------
  # sizes of the standardizing (control) and the other (experimental) group;
  # the group supplying the sd defines the reference scale and the df
  nC <- if (useControlSd) length(y) else length(x)
  nE <- if (useControlSd) length(x) else length(y)

  if (nC < 2L || nE < 2L)
    stop("'x' and 'y' must each contain at least 2 non-missing values")

  sdC <- if (useControlSd) sd(y) else sd(x)

  if (!is.finite(sdC) || sdC <= 0)
    stop("standard deviation of the standardizing group must be finite and positive")

  delta <- (mean(x) - mean(y)) / sdC

  # exact small-sample bias correction factor J(df), df = nC - 1
  if (correct) {
    if (nC < 3L)
      stop("bias correction requires at least 3 observations in the standardizing group")
    dfC <- nC - 1
    cf  <- exp(lgamma(dfC / 2) - log(sqrt(dfC / 2)) - lgamma((dfC - 1) / 2))
  } else {
    cf <- 1
  }

  # -- confidence interval --------------------------------------------------
  if (is.na(conf.level)) {
    res <- c(est = cf * delta)

  } else {
    tObs <- delta * sqrt(nC * nE / (nC + nE))
    # unname(): the merged .nctCI() returns c(lci = , uci = ), while the
    # copy that used to live in this file returned a bare pair. The call
    # site still wrote lci = lim[1L], so the names composed into
    # "lci.lci" and res["lci"] became NA. Same lesson as .toWallClock():
    # when a shared helper changes shape, every caller has to be checked,
    # not just the one being fixed.
    lim  <- unname(.nctCI(tObs, df = nC - 1, conf.level = conf.level,
                          sides = sides))
    scl  <- sqrt((nC + nE) / (nC * nE))

    res <- cf * c(est = delta, lci = lim[1L] * scl, uci = lim[2L] * scl)
  }

  attr(res, "magnitude") <- c("negligible", "small", "medium", "large")[
                              findInterval(abs(res[[1L]]), c(0.2, 0.5, 0.8)) + 1L]

  return(res)
}



# == internal helper functions ===================================================

# .nctCI() and .nctRoot() used to live here. They collided with a second,
# differently-shaped .nctCI() in coefVar.R; both now come from nctCI.R.
