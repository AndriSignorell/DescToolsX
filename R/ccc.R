
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
#' `"z-transform"` method rescales it to the z scale via the delta
#' method, where \eqn{d\,\mathrm{atanh}(\rho)/d\rho = 1/(1 - \rho^2)}.
#'
#' `sides` names the side on which the finite bound lies:
#' `"left"` yields an interval bounded below, with the upper limit
#' fixed at 1, and `"right"` one bounded above, with the lower limit
#' fixed at -1. 
#'
#' Missing values are handled according to package conventions:
#' if `na.rm = FALSE` and either `x` or `y` contains missing
#' values, `NA` is returned. If `na.rm = TRUE`, complete cases are
#' used. Infinite values carry no comparable convention - they leave the
#' moments undefined and are rejected with an error.
#'
#' @param x a numeric vector
#' @param y a numeric vector of equal length to `x`
#' 
#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned. With perfect
#'   concordance or discordance the asymptotic variance is zero; the
#'   analytic methods then return `NA` bounds with a warning.
#'   One-sided intervals require `conf.level > 0.5`.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()].
#'
#' @param method a character string specifying the confidence interval
#' method. One of `"z-transform"`, `"boot"`, or
#' `"asymptotic"`.
#' 
#' @param na.rm logical; if `TRUE`, incomplete observation pairs are
#' removed before computation
#' @param ... additional arguments controlling the bootstrap procedure.
#' Currently `R`, `parallel` and `ncpus` are supported.
#'
#' @return a named numeric vector containing only `est` when
#' `conf.level = NA`; otherwise a named numeric vector with elements:
#' \describe{
#'   \item{`est`}{point estimate.}
#'   \item{`lci`}{lower confidence interval bound.}
#'   \item{`uci`}{upper confidence interval bound.}
#' }
#'
#' Additional diagnostics are stored as attributes:
#'
#' \describe{
#'   \item{`nObs`}{number of observations used}
#'   \item{`scaleShift`}{scale shift parameter}
#'   \item{`locationShift`}{location shift parameter}
#'   \item{`biasCorrection`}{bias correction factor}
#'   \item{`method`}{confidence interval method, if applicable}
#'   \item{`confLevel`}{confidence level, if applicable}
#'   \item{`sides`}{confidence interval type, if applicable}
#' }
#'
#' @references
#' Lin, L. I.-K. (1989). A concordance correlation coefficient to evaluate
#' reproducibility. *Biometrics*, *45*(1), 255-268.
#'
#' Lin, L. I.-K. (2000). A note on the concordance correlation coefficient.
#' *Biometrics*, *56*(1), 324-325.
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

  checkConfLevel(conf.level)

  # one-sided intervals are computed at the two-sided level
  # 2 * conf.level - 1, which must stay positive
  if(!is.na(conf.level) && sides != "two.sided" && conf.level <= 0.5)
    stop("Argument 'conf.level' must exceed 0.5 for a one-sided interval.")

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

  # Shape consistency: every other exit returns the est/lci/uci vector
  # with its diagnostic attributes, so a bare NA_real_ here forced callers
  # to type-check the result before they could index it.
  if(anyNA(x) || anyNA(y)) {

    if(is.na(conf.level))
      return(.makeEstimateResult(est = NA_real_,
                                 attrs = list(nObs = length(x))))

    return(.makeEstimateResult(est = NA_real_,
                               lci = NA_real_,
                               uci = NA_real_,
                               attrs = list(nObs = length(x),
                                            method = method,
                                            confLevel = conf.level,
                                            sides = sides)))
  }

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

  # Every method computes a two-sided interval; a one-sided one is the
  # two-sided interval at level 2 * conf.level - 1 with its uninformative
  # side opened by applySides() - the finite bound is the same either way.
  confAdj <- if(sides == "two.sided") conf.level else 2 * conf.level - 1
  alpha <- 1 - confAdj

  if(method == "boot") {

    dots <- list(...)
    bootArgs <- .extractBootArgs(dots)

    statFun <- function(data, idx) {

      .cccPoint(
        data[idx, 1],
        data[idx, 2]
      )

    }

    # ncpus was extracted but never handed on, so parallel = "multicore"
    # silently ran on a single core.
    bootObj <- boot::boot(
      data = cbind(x, y),
      statistic = statFun,
      R = bootArgs$R,
      parallel = bootArgs$parallel,
      ncpus = bootArgs$ncpus
    )

    ci <- unname(
      quantile(
        bootObj$t,
        probs = c(alpha / 2, 1 - alpha / 2),
        na.rm = TRUE
      )
    )

  } else {

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

    # A zero standard error (x == y, or y == -x about a common mean) would
    # collapse the interval onto the estimate, which excludes every other
    # value - no finite sample supports that. NA bounds with a warning, as
    # in cramerV, spearmanCor, kappaM and corCI.
    if(se == 0) {

      warning("the asymptotic variance of the CCC is zero (perfect ",
              "concordance or discordance); no confidence interval",
              call. = FALSE)

      ci <- c(NA_real_, NA_real_)

    } else {

      zCrit <- qnorm(1 - alpha / 2)

      ci <- if(method == "asymptotic") {

        rhoC + c(-1, 1) * zCrit * se

      } else {

        # Avoid infinities in Fisher's z-transformation.
        rhoAdj <- pmin(
          pmax(rhoC, -1 + sqrt(.Machine$double.eps)),
          1 - sqrt(.Machine$double.eps)
        )

        # Delta-method variance transformation:
        # d atanh(rho) / d rho = 1 / (1 - rho^2)
        seZ <- se / (1 - rhoAdj^2)

        fisherZInv(fisherZ(rhoAdj) + c(-1, 1) * zCrit * seZ)

      }

    }

  }

  # clamps to [-1, 1] (the asymptotic interval can leave it) and opens
  # the uninformative side at the range boundary
  ci <- applySides(ci, sides, lo = -1, hi = 1)

  attrs$method <- method
  attrs$confLevel <- conf.level
  attrs$sides <- sides

  .makeEstimateResult(
    est = rhoC,
    lci = ci[["lci"]],
    uci = ci[["uci"]],
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
