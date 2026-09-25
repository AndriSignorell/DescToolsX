
#' Brier Score
#'
#' Computes the Brier score for binary probabilistic predictions, optionally
#' with a confidence interval via a normal approximation or bootstrap.
#'
#' @details
#' The Brier score is defined as
#' \deqn{BS = \frac{1}{n}\sum_{i=1}^n
#'   \bigl[y_i(1-\hat p_i)^2 + (1-y_i)\hat p_i^2\bigr]}{
#'   BS = mean(y * (1 - p)^2 + (1 - y) * p^2)}
#' where \eqn{y_i \in \{0,1\}} and \eqn{\hat p_i} is the predicted
#' probability.  Lower is better; a perfect model scores 0.
#'
#' The scaled Brier score (`scaled = TRUE`) expresses skill relative
#' to the climatological baseline \eqn{BS_{\max}}, yielding 1 for a
#' perfect model and 0 for the no-skill reference.
#'
#' `sides` names the side on which the finite bound lies. The open side is
#' reported at the boundary of the score's range, which is \eqn{[0, 1]}
#' for the raw and \eqn{(-\infty, 1]} for the scaled score: `"left"`
#' yields \eqn{[lci, 1]}, `"right"` yields \eqn{[0, uci]} (raw) or
#' \eqn{(-\infty, uci]} (scaled). Two-sided intervals are clamped to the
#' same range. One-sided intervals require `conf.level > 0.5`.
#'
#' **Normal interval** (`method = "normal"`)
#'
#' A delta-method normal approximation based on the variance of the
#' per-observation Brier losses.  Fast and deterministic; reliable for
#' moderate to large samples.  With `scaled = TRUE` the standard error
#' is carried onto the skill scale by dividing through \eqn{BS_{\max}},
#' which is treated as fixed; the interval therefore ignores the sampling
#' variability of the baseline and is mildly anti-conservative. Prefer
#' `method = "boot"` for scaled scores.
#'
#' **Bootstrap interval** (`method = "boot"`)
#'
#' Case-resampling bootstrap via `brier_boot_cpp()`.  The bootstrap
#' type is controlled by the `type` argument (passed through
#' `...`):
#' \describe{
#'   \item{`"bca"`}{bias-corrected and accelerated (default).
#'     Most accurate; requires \eqn{R \geq 200}.}
#'   \item{`"perc"`}{percentile interval}
#'   \item{`"norm"`}{normal approximation using the bootstrap standard
#'     error}
#' }
#' Further bootstrap arguments passed through `...` via
#' `.extractBootArgs()`:
#' \describe{
#'   \item{`R`}{number of bootstrap replicates (default `999`)}
#'   \item{`parallel`}{parallelisation: `"no"`,
#'     `"multicore"`, or `"snow"` (default `"no"`)}
#'   \item{`ncpus`}{number of CPUs (default
#'     `getOption("boot.ncpus", 1L)`)}
#' }
#'
#' @param x       either a numeric vector of observed binary outcomes
#'   (\eqn{0}/\eqn{1}) when `pred` is supplied, or a fitted model
#'   object (`glm` or similar) from which both response and
#'   predictions are extracted
#' @param pred    a numeric vector of predicted probabilities in
#'   \eqn{[0,1]}. Required when `x` is a numeric vector; ignored
#'   when `x` is a model object.
#' @param scaled  logical. Should the scaled Brier score be returned?
#'   Default `FALSE`.
#'   
#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()].
#' @param method  confidence interval method: `"normal"` (delta-method
#'   approximation, default) or `"boot"` (bootstrap via
#'   `brier_boot_cpp()`)
#' @param ...     further arguments passed to the bootstrap engine when
#'   `method = "boot"`: `R`, `type` (`"perc"`, `"basic"`,
#'   `"norm"` or `"bca"`; `"stud"` is not available). `parallel` and
#'   `ncpus` are accepted but have no effect: the resampling loop in
#'   `brier_boot_cpp()` is serial. See Details.
#'
#' @return if `conf.level = NA`, a numeric scalar containing the Brier
#' score; otherwise a named numeric vector with elements:
#' \describe{
#'   \item{`est`}{point estimate of the Brier score.}
#'   \item{`lci`}{lower confidence interval bound.}
#'   \item{`uci`}{upper confidence interval bound.}
#' }
#'
#' @seealso [stats::predict()]
#'
#' @examples
#' set.seed(1)
#' resp <- rbinom(200, 1, 0.4)
#' pred <- plogis(rnorm(200, ifelse(resp == 1, 0.5, -0.5)))
#'
#' brierScore(resp, pred)
#' brierScore(resp, pred, conf.level = 0.95)
#' brierScore(resp, pred, conf.level = 0.95, method = "boot", type = "bca")
#' brierScore(resp, pred, conf.level = 0.95, scaled = TRUE)
#'
#' @family model.metrics
#' @concept model-evaluation
#' @concept calibration
#' @export
brierScore <- function(x,
                       pred       = NULL,
                       conf.level = NA,
                       sides      = c("two.sided", "left", "right"),
                       method     = c("normal", "boot"),
                       scaled     = FALSE,
                       ...) {

  sides  <- match.arg(sides)
  method <- match.arg(method)
  # checked before is.na() below: NULL or c(0.9, 0.95) broke that if()
  # with an internal error, and NaN quietly suppressed the interval
  checkConfLevel(conf.level)
  checkFlag(scaled)

  # --- extract resp / pred ---------------------------------------------
  if (!is.null(pred)) {
    resp <- x
  } else {
    if (inherits(x, "glm")) {
      pred <- predict(x, type = "response")
      resp <- .numResponse(x)
    } else {
      pred <- predict(x, type = "prob")[, 2L]
      resp <- .numResponse(x)
    }
  }

  # --- validate resp / pred --------------------------------------------
  if (length(resp) != length(pred))
    stop("'x' and 'pred' must have the same length.")
  if (anyNA(resp) || anyNA(pred))
    stop("'x' and 'pred' must not contain missing values.")
  if (!all(resp %in% c(0L, 1L)))
    stop("'x' (response) must be binary (0/1).")
  if (any(pred < 0 | pred > 1))
    stop("'pred' must contain probabilities in [0, 1].")

  # the scaled score divides by the Brier score of the prevalence, which
  # is 0 for a constant response: the point estimate was -Inf or NaN
  # without comment, only the interval branch checked it
  if (scaled && length(unique(resp)) < 2L)
    stop("the scaled Brier score is undefined: the response has no variation.")

  # --- point estimate --------------------------------------------------
  bsHat <- .brierLoss(resp, pred, scaled)

  if (is.na(conf.level))
    return(bsHat)

  # --- CI setup --------------------------------------------------------
  # A one-sided interval puts the full alpha on its single finite side, so
  # the two-sided machinery below is run at a doubled alpha and the
  # irrelevant bound opened afterwards by applySides().
  if (sides != "two.sided" && conf.level <= 0.5)
    stop("'conf.level' must exceed 0.5 for a one-sided interval.")
  confAdj <- if (sides != "two.sided") 2 * conf.level - 1 else conf.level
  alpha   <- 1 - confAdj
  n       <- length(resp)

  bootType <- NA_character_

  ci <- switch(method,

               normal = {
                 loss <- resp * (1 - pred)^2 + (1 - resp) * pred^2
                 se   <- sqrt(var(loss) / n)

                 # se above is on the raw Brier scale. When bsHat is the
                 # SCALED score the two live on different scales entirely,
                 # and the former code combined them directly - producing
                 # an interval whose width had nothing to do with the
                 # estimate. Delta method with BSmax held fixed.
                 if (scaled) {
                   meanY <- mean(resp)
                   se <- se / (meanY * (1 - meanY)^2 + (1 - meanY) * meanY^2)
                 }

                 z <- qnorm(1 - alpha / 2)
                 c(bsHat - z * se, bsHat + z * se)
               },

               boot = {
                 bootArgs <- .extractBootArgs(list(...))
                 bootType <- bootArgs$type

                 bootVals <- as.numeric(
                   brier_boot_cpp(resp, pred, bootArgs$R, scaled))

                 # A resample with a constant response has no scaled
                 # score; the C++ side returns NA for it instead of the
                 # -Inf/NaN that made quantile() fail.
                 nBad <- sum(is.na(bootVals))
                 if (nBad == length(bootVals))
                   stop("all bootstrap resamples have a constant response; ",
                        "the scaled Brier score is undefined for them.")
                 if (nBad > 0L) {
                   warning(nBad, " of ", length(bootVals), " bootstrap ",
                           "resamples had a constant response and were ",
                           "dropped", call. = FALSE)
                   bootVals <- bootVals[!is.na(bootVals)]
                 }

                 switch(bootType,

                        perc = {
                          # alpha/2, not alpha: alpha has already been
                          # doubled above for one-sided requests, so using
                          # it undivided delivered a 90% bound where 95%
                          # was asked for. The open side is set to +/-Inf
                          # below like every other method, rather than to
                          # the extreme order statistic of the resamples.
                          quantile(bootVals, probs = c(alpha / 2, 1 - alpha / 2),
                                   names = FALSE)
                        },

                        basic = {
                          # basic (reverse percentile): 2 * est - quantiles
                          2 * bsHat - quantile(bootVals,
                                               probs = c(1 - alpha / 2, alpha / 2),
                                               names = FALSE)
                        },

                        # 'stud' needs a standard error per resample, which
                        # the C++ loop does not compute; the switch returned
                        # NULL for it and lci came back as NA
                        stud = stop("type = \"stud\" is not available for ",
                                    "brierScore(); use \"perc\", \"basic\", ",
                                    "\"norm\" or \"bca\".", call. = FALSE),

                        norm = {
                          seBoot <- sd(bootVals)
                          z      <- qnorm(1 - alpha / 2)
                          c(bsHat - z * seBoot, bsHat + z * seBoot)
                        },

                        bca = {
                          pLess <- mean(bootVals < bsHat)
                          if (pLess <= 0 || pLess >= 1)
                            stop("BCa bias correction is not finite; ",
                                 "use type = \"perc\" or increase R.")
                          z0 <- qnorm(pLess)

                          jack <- vapply(seq_len(n),
                                         function(i) .brierLoss(resp[-i], pred[-i], scaled),
                                         numeric(1L))
                          jackMean <- mean(jack)
                          num <- sum((jackMean - jack)^3)
                          den <- 6 * sum((jackMean - jack)^2)^(3 / 2)
                          a   <- if (den == 0) 0 else num / den

                          zAlpha <- qnorm(c(alpha / 2, 1 - alpha / 2))
                          adj <- pnorm(z0 + (z0 + zAlpha) / (1 - a * (z0 + zAlpha)))
                          quantile(bootVals, probs = adj, names = FALSE)
                        }
                 )
               }
  )

  # --- range and sides -------------------------------------------------
  # The raw score lies in [0, 1], the scaled one in (-Inf, 1]. The open
  # side is reported at that boundary (it used to be +/-Inf regardless),
  # and the two-sided interval is clamped to it.
  c(est = bsHat,
    applySides(ci, sides, lo = if (scaled) -Inf else 0, hi = 1))
}


# == internal helper functions ===============================================

# --- internal: Brier loss per observation and score ------------------

.brierLoss <- function(resp, pred, scaled = FALSE) {
  loss <- resp * (1 - pred)^2 + (1 - resp) * pred^2
  bs   <- mean(loss)
  if (scaled) {
    meanY <- mean(resp)
    bsMax <- meanY * (1 - meanY)^2 + (1 - meanY) * meanY^2
    bs    <- 1 - bs / bsMax
  }
  bs
}



# --- internal: extract response from model object --------------------

# glm stores the (numeric) response in $y, which is exactly what is
# needed here. The former version instead overwrote obj$terms with
# eval(obj$call$formula) before calling model.frame(): the assignment
# replaced a valid terms object with a bare formula, and the eval() ran in
# this function's frame, so a call built from a formula held in a variable
# ("object not found") or fitted with model = FALSE would break.
.numResponse <- function(obj) {

  if (!is.null(obj$y))
    return(as.numeric(obj$y))

  res <- model.response(model.frame(obj))
  if (is.factor(res)) res <- as.numeric(res) - 1
  as.numeric(res)
}
