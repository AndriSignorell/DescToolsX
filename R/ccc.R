
#' Lin's Concordance Correlation Coefficient
#'
#' Computes Lin's concordance correlation coefficient (CCC) for assessing
#' agreement between two continuous measurements.
#'
#' The CCC combines measures of precision and accuracy and quantifies the
#' deviation of the observed data from the line of perfect concordance.
#' Values range from -1 to 1, where 1 indicates perfect agreement.
#'
#' Confidence intervals can be computed using a Fisher z-transformation,
#' a nonparametric bootstrap, or the asymptotic approximation of
#' Lin (2000). The asymptotic variance implemented here is the corrected
#' form given by Lin (2000), superseding the expression in Lin (1989).
#' Internally it is held on the scale of \eqn{\rho_c} itself; the
#' \code{"z-transform"} method rescales it to the z scale via the delta
#' method, where \eqn{d\,\mathrm{atanh}(\rho)/d\rho = 1/(1 - \rho^2)}.
#'
#' \code{sides} names the side on which the finite bound lies:
#' \code{"left"} yields an interval bounded below, with the upper limit
#' fixed at 1, and \code{"right"} one bounded above, with the lower limit
#' fixed at -1. Note that this is the reverse of the convention in
#' \pkg{DescTools}, where \code{sides} follows the alternative hypothesis of
#' \code{\link[stats]{t.test}}.
#'
#' Missing values are handled according to package conventions:
#' if \code{na.rm = FALSE} and either \code{x} or \code{y} contains missing
#' values, \code{NA} is returned. If \code{na.rm = TRUE}, complete cases are
#' used. Infinite values carry no comparable convention - they leave the
#' moments undefined and are rejected with an error.
#'
#' @param x a numeric vector
#' @param y a numeric vector of equal length to \code{x}
#' @param conf.level a single confidence level for the returned confidence
#' interval. Set to \code{NA} (default) to suppress confidence interval
#' calculation.
#' @param sides a character string specifying a two-sided or one-sided
#' confidence interval
#' @param method a character string specifying the confidence interval
#' method. One of \code{"z-transform"}, \code{"boot"}, or
#' \code{"asymptotic"}.
#' @param na.rm logical; if \code{TRUE}, incomplete observation pairs are
#' removed before computation
#' @param ... additional arguments controlling the bootstrap procedure.
#' Currently \code{R} and \code{parallel} are supported.
#'
#' @return a named numeric vector containing only \code{est} when
#' \code{conf.level = NA}; otherwise a named numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate.}
#'   \item{\code{lci}}{lower confidence interval bound.}
#'   \item{\code{uci}}{upper confidence interval bound.}
#' }
#'
#' Additional diagnostics are stored as attributes:
#'
#' \describe{
#'   \item{\code{nObs}}{number of observations used}
#'   \item{\code{scaleShift}}{scale shift parameter}
#'   \item{\code{locationShift}}{location shift parameter}
#'   \item{\code{biasCorrection}}{bias correction factor}
#'   \item{\code{method}}{confidence interval method, if applicable}
#'   \item{\code{confLevel}}{confidence level, if applicable}
#'   \item{\code{sides}}{confidence interval type, if applicable}
#' }
#'
#' @section Random number generation:
#' \code{method = "boot"} draws bootstrap resamples and therefore depends on
#' the state of R's global random number generator. No seed is set
#' internally; the global RNG state advances normally as resamples are
#' drawn. Call \code{\link[base]{set.seed}} beforehand for reproducible
#' results.
#'
#' @references
#' Lin, L. I.-K. (1989). A concordance correlation coefficient to evaluate
#' reproducibility. \emph{Biometrics}, \emph{45}(1), 255-268.
#'
#' Lin, L. I.-K. (2000). A note on the concordance correlation coefficient.
#' \emph{Biometrics}, \emph{56}(1), 324-325.
#'
#' @examples
#' set.seed(123)
#'
#' x <- rnorm(100)
#' y <- x + rnorm(100, sd = 0.2)
#'
#' ccc(x, y)
#'
#' ccc(x, y, conf.level = 0.95)
#'
#' ccc(
#'   x, y,
#'   conf.level = 0.95,
#'   method = "boot",
#'   R = 999
#' )
#'
#' @family assoc.agreement
#' @concept method-comparison
#' @concept correlation
#' @concept reliability
#'
#' @export
ccc <- function(
    x,
    y,
    conf.level = NA,
    sides = c("two.sided", "left", "right"),
    method = c("z-transform", "boot", "asymptotic"),
    na.rm = FALSE,
    ...
){

  if(!is.numeric(x) || !is.null(dim(x)))
    stop("Argument 'x' must be a numeric vector.")

  if(!is.numeric(y) || !is.null(dim(y)))
    stop("Argument 'y' must be a numeric vector.")

  if(length(x) != length(y))
    stop("Arguments 'x' and 'y' must have equal length.")

  if(!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("Argument 'na.rm' must be a single non-missing logical value.")

  sides <- match.arg(sides)
  method <- match.arg(method)

  # Checked for length before is.na(), which would otherwise be passed a
  # zero-length or multi-element value and make the if() below fail with
  # an internal condition-length error rather than a clear message.
  if(!is.numeric(conf.level) && !is.logical(conf.level))
    stop("Argument 'conf.level' must be a single number between 0 and 1, or NA.")

  if(length(conf.level) != 1L)
    stop("Argument 'conf.level' must be a single number between 0 and 1, or NA.")

  # NaN is numeric and NA-like, but suppressing the interval on a NaN
  # confidence level would hide a caller error rather than express an
  # intent to omit it, so only a true NA does that.
  if(is.nan(conf.level))
    stop("Argument 'conf.level' must be a single number between 0 and 1, or NA.")

  if(!is.na(conf.level)) {

    if(!is.numeric(conf.level) ||
       !is.finite(conf.level) ||
       conf.level <= 0 ||
       conf.level >= 1) {

      stop(
        "Argument 'conf.level' must be a single number between 0 and 1."
      )

    }

  }

  if(na.rm) {

    keep <- complete.cases(x, y)

    x <- x[keep]
    y <- y[keep]

    if(length(x) < 3L) {

      stop(
        "Arguments 'x' and 'y' retain fewer than 3 complete observation pairs after removing missing values."
      )

    }

  }

  # The length requirement is structural and applies whether or not the
  # data are complete, so it is checked before the NA short-circuit below.
  # Otherwise ccc(c(1, NA), c(2, NA)) would return NA while the equally
  # short ccc(c(1, 2), c(2, 3)) errors - an inconsistency in what is
  # nominally the same failure.
  if(length(x) < 3L)
    stop("Arguments 'x' and 'y' must have at least 3 observations.")

  if(anyNA(x) || anyNA(y))
    return(NA_real_)

  # Checked only after the NA policy has been applied: is.finite() is
  # FALSE for NA too, so an earlier check would turn the documented
  # NA-return into an error. Inf carries no such convention - it makes
  # the moments undefined and is rejected outright.
  if(!all(is.finite(x)))
    stop("Argument 'x' must not contain infinite values.")

  if(!all(is.finite(y)))
    stop("Argument 'y' must not contain infinite values.")

  .cccEngine(
    x = x,
    y = y,
    conf.level = conf.level,
    sides = sides,
    method = method,
    ...
  )

}


.cccEngine <- function(
    x,
    y,
    conf.level,
    sides,
    method,
    ...
){

  nObs <- length(x)

  if(nObs < 3L)
    stop("Arguments 'x' and 'y' must have at least 3 complete observations.")

  sx2 <- var(x) * (nObs - 1) / nObs
  sy2 <- var(y) * (nObs - 1) / nObs

  if(sx2 <= 0)
    stop("Argument 'x' must have positive variance.")

  if(sy2 <= 0)
    stop("Argument 'y' must have positive variance.")

  xb <- mean(x)
  yb <- mean(y)

  sdx <- sqrt(sx2)
  sdy <- sqrt(sy2)

  r <- cor(x, y)

  rhoC <- .cccPoint(x, y)

  geomMeanSd <- (sx2 * sy2)^0.25

  scaleShift <- sdy / sdx

  locationShift <-
    (yb - xb) / geomMeanSd

  # Lin's bias correction factor C_b. The equivalent closed form
  # 2 / (v + 1/v + u^2) is used in preference to rhoC / r: it is
  # algebraically identical but has no division by r, so it stays finite
  # when x and y are near-uncorrelated instead of producing NA.
  biasCorrection <-
    2 / (scaleShift + 1 / scaleShift + locationShift^2)

  attrs <- list(
    nObs = nObs,
    scaleShift = scaleShift,
    locationShift = locationShift,
    biasCorrection = biasCorrection
  )

  if(is.na(conf.level)) {

    return(
      .makeEstimateResult(
        est = rhoC,
        attrs = attrs
      )
    )

  }

  alpha <- 1 - conf.level

  if(method == "boot") {

    dots <- list(...)
    bootArgs <- .extractBootArgs(dots)

    statFun <- function(data, idx) {

      .cccPoint(
        data[idx, 1],
        data[idx, 2]
      )

    }

    bootObj <- boot::boot(
      data = cbind(x, y),
      statistic = statFun,
      R = bootArgs$R,
      parallel = bootArgs$parallel
    )

    # Only the informative bound is taken from the resampling
    # distribution; the other is fixed at the parameter boundary, so no
    # degenerate 0 %/100 % quantile is requested. sides names the side on
    # which the finite bound lies.
    probs <- switch(
      sides,
      "two.sided" = c(alpha / 2, 1 - alpha / 2),
      "left"      = alpha,
      "right"     = conf.level
    )

    ci <- unname(
      quantile(
        bootObj$t,
        probs = probs,
        na.rm = TRUE
      )
    )

    if(sides == "two.sided") {

      lci <- ci[1]
      uci <- ci[2]

    } else if(sides == "left") {

      lci <- ci[1]
      uci <- 1

    } else {

      lci <- -1
      uci <- ci[1]

    }

  } else {

    zCrit <- if(sides == "two.sided")
      qnorm(1 - alpha / 2)
    else
      qnorm(conf.level)

    # Lin's (2000) asymptotic variance, expressed on the scale of rhoC
    # rather than on the z scale: the bracketed term equals the z-scale
    # variance multiplied through by (1 - rhoC^2)^2. The "asymptotic"
    # method uses it directly; the "z-transform" method divides it back
    # out below to recover the z-scale standard error.
    #
    # Written via the identity rhoC = r * biasCorrection so that no
    # division by r or r^2 remains. The textbook form is algebraically
    # identical but evaluates to 0/0 for uncorrelated (yet non-constant)
    # data, where r = rhoC = 0 is a perfectly regular case; this form
    # returns the correct limit biasCorrection^2 / (nObs - 2) instead.
    varRho <- (
      (1 - r^2) * biasCorrection^2 * (1 - rhoC^2) +
        2 * rhoC^2 * biasCorrection * (1 - rhoC) *
          locationShift^2 -
        0.5 * rhoC^2 * biasCorrection^2 * locationShift^4
    ) / (nObs - 2)

    se <- sqrt(max(varRho, 0))

    if(method == "asymptotic") {

      if(sides == "two.sided") {

        lci <- rhoC - zCrit * se
        uci <- rhoC + zCrit * se

      } else if(sides == "left") {

        lci <- rhoC - zCrit * se
        uci <- 1

      } else {

        lci <- -1
        uci <- rhoC + zCrit * se

      }

      lci <- max(lci, -1)
      uci <- min(uci,  1)

    } else {

      # A zero standard error means the estimate is degenerate (e.g.
      # x == y, giving rhoC == 1). The clamping applied to rhoAdj below
      # would otherwise return a limit just short of the true value,
      # so the interval collapses onto the estimate directly.
      if(se == 0) {

        if(sides == "two.sided") {

          lci <- rhoC
          uci <- rhoC

        } else if(sides == "left") {

          lci <- rhoC
          uci <- 1

        } else {

          lci <- -1
          uci <- rhoC

        }

      } else {

        # Avoid infinities in Fisher's z-transformation.
        rhoAdj <- pmin(
          pmax(rhoC, -1 + sqrt(.Machine$double.eps)),
          1 - sqrt(.Machine$double.eps)
        )

        # Delta-method variance transformation:
        # d atanh(rho) / d rho = 1 / (1 - rho^2)
        z <- fisherZ(rhoAdj)

        seZ <- se / (1 - rhoAdj^2)

        if(sides == "two.sided") {

          lci <- fisherZInv(
            z - zCrit * seZ
          )

          uci <- fisherZInv(
            z + zCrit * seZ
          )

        } else if(sides == "left") {

          lci <- fisherZInv(
            z - zCrit * seZ
          )

          uci <- 1

        } else {

          lci <- -1

          uci <- fisherZInv(
            z + zCrit * seZ
          )

        }

      }

    }

  }

  attrs$method <- method
  attrs$confLevel <- conf.level
  attrs$sides <- sides

  .makeEstimateResult(
    est = rhoC,
    lci = lci,
    uci = uci,
    attrs = attrs
  )

}



.cccPoint <- function(x, y){

  nObs <- length(x)

  # cov() rather than cor(): a constant bootstrap resample makes cor()
  # return NA with a warning, discarding an otherwise usable replicate
  # for which the CCC is regularly 0. Rescaled from the (n-1) to the (n)
  # denominator to match the moments used in the CCC definition.
  sx2 <- var(x) * (nObs - 1) / nObs
  sy2 <- var(y) * (nObs - 1) / nObs
  sxy <- cov(x, y) * (nObs - 1) / nObs

  denom <- sx2 + sy2 + (mean(y) - mean(x))^2

  # Only reachable when x and y are constant and identical, in which case
  # concordance is undefined rather than perfect.
  if(denom == 0)
    return(NA_real_)

  2 * sxy / denom

}
