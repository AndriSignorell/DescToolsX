
#' Concordance Statistic (C-Statistic / AUC)
#'
#' Computes the concordance statistic (C-statistic), equivalent to the
#' area under the ROC curve (AUC), for predicted values and a binary outcome.
#'
#' @param x an object for which the C-statistic should be computed; for the
#' default method, a numeric vector of predicted values
#' @param resp binary response vector
#' @param conf.level confidence level for the interval; `NA` (default)
#' suppresses interval calculation
#' @param ... additional arguments passed to methods
#'
#' @return if `conf.level = NA`, an unnamed numeric scalar between 0
#' and 1; otherwise a named numeric vector with elements:
#' \describe{
#'   \item{`est`}{point estimate of the C-statistic.}
#'   \item{`lci`}{lower confidence interval bound.}
#'   \item{`uci`}{upper confidence interval bound.}
#' }
#'
#' @name cStat
#'
#' @details
#' The C-statistic is defined as the probability that, for a randomly chosen
#' pair of observations with different outcomes, the observation with the
#' higher predicted value has the higher observed outcome.
#'
#' Ties in predicted values are handled by assigning a weight of 0.5.
#'
#' `resp` is converted with `as.numeric(factor(resp)) - 1`, so
#' the **second** level in sort order counts as the event - `1`
#' for a 0/1 coding, `TRUE` for a logical, and the second factor level
#' otherwise. Getting this backwards returns \eqn{1 - C} rather than an
#' error, so check the level order when the response is a factor with
#' unusual labels.
#'
#' This implementation uses:
#' \itemize{
#'   \item O(n log n) concordance computation
#'   \item Parallel bootstrap confidence intervals via \pkg{RcppParallel}
#'   \item Efficient memory handling
#' }
#'
#' The number of bootstrap samples can be supplied through `...` as
#' `R`; the default is 1000.
#'
#' @section Random number generation:
#' A confidence level triggers a bootstrap, which draws a seed from R's
#' global random number generator and therefore advances it. Call
#' [base::set.seed()] beforehand for reproducible intervals.
#'
#' @examples
#' # Default method
#' set.seed(1)
#' x <- runif(100)
#' y <- rbinom(100, 1, 0.5)
#' cStat(x, resp = y)
#'
#' # GLM method
#' r.mod <- glm(complaint ~ temperature + wrongpizza + wine_ordered,
#'              data = bedrock::Pizza, family = binomial)
#' cStat(r.mod, conf.level = 0.95)
#'
#' @family assoc.ordinal
#' @concept association-measure
#' @concept ordinal
#' @concept roc
#' @export
cStat <- function(x, ...) {
  UseMethod("cStat")
}


#' @method cStat glm
#' @rdname cStat
#' @export
#' @importFrom stats predict model.response model.frame
cStat.glm <- function(x, ...) {

  # glm keeps the numeric response in $y; falling back to the model frame
  # only when the fit was made with y = FALSE. model.response(x$model)
  # alone failed outright for model = FALSE fits.
  resp <- if (!is.null(x$y)) x$y else model.response(model.frame(x))

  cStat.default(
    x = predict(x, type = "response"),
    resp = resp,
    ...
  )
}


#' @method cStat default
#' @rdname cStat
#' @param resp a binary response vector (numeric, logical, or factor)
#' @export
cStat.default <- function(x, resp, conf.level = NA, ...) {

  if (missing(resp)) {
    stop("`resp` must be provided for the default method.", call. = FALSE)
  }

  # --- basic checks ---
  if (length(x) != length(resp)) {
    stop("`x` and `resp` must have the same length.", call. = FALSE)
  }

  if (anyNA(x) || anyNA(resp)) {
    stop("`x` and `resp` must not contain missing values.", call. = FALSE)
  }

  # convert response to numeric {0,1}
  y <- as.numeric(factor(resp)) - 1

  if (!all(y %in% c(0, 1))) {
    stop("`resp` must be binary.", call. = FALSE)
  }

  # A constant response has no discordant/concordant pairs at all, so the
  # ratio below is 0/0. Previously this returned NaN without comment.
  if (length(unique(y)) < 2L) {
    stop("`resp` must contain both outcome classes.", call. = FALSE)
  }

  # --- compute concordance ---
  z <- condis_pairs_xy_cpp(y, x)

  # unname(): the result inherited the name "C" from z["C"], so the
  # documented "numeric scalar" came back labelled
  est <- unname((z["C"] + 0.5 * z["Ties_Y"]) / (z["D"] + z["C"] + z["Ties_Y"]))

  if (is.na(conf.level))
    return(est)

  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
      conf.level <= 0 || conf.level >= 1)
    stop("`conf.level` must be a single number in (0, 1), or NA.",
         call. = FALSE)

  seed <- sample.int(.Machine$integer.max, 1)

  R <- list(...)$R %||% 1000

  ci <- cstat_boot_cpp(y, x, R,
                       alpha = 1 - conf.level,
                       seed = seed)

  # est comes from the exact O(n log n) pass above, not from the bootstrap
  # output, so that cStat(x, resp) and cStat(x, resp, conf.level = 0.95)
  # cannot report two different point estimates
  c(est = est, lci = unname(ci[2]), uci = unname(ci[3]))
}
