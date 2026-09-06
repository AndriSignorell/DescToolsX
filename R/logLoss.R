#' Log Loss
#'
#' Computes the logarithmic loss (cross-entropy loss) for binary
#' probabilistic predictions.
#'
#' @details
#' Log loss is defined as
#'
#' \deqn{LL = -\frac{1}{n}\sum_{i=1}^n
#'   \left[y_i \log(\hat p_i) + (1-y_i)\log(1-\hat p_i)\right],}{
#'   LL = -mean(y * log(p) + (1-y) * log(1-p))}
#'
#' where \eqn{y_i \in \{0,1\}} and \eqn{\hat p_i} is the predicted
#' probability of the positive class. Lower values indicate better
#' probabilistic predictions; a perfect model has log loss 0.
#'
#' Before taking logarithms the predictions are clipped into the closed
#' interval \eqn{[\epsilon, 1-\epsilon]}, so that a prediction of exactly 0
#' or 1 contributes a finite penalty of at most \eqn{-\log(\epsilon)}
#' (about 36.0 for the default \code{eps}) instead of \code{Inf}.
#'
#' Being the negative Bernoulli log-likelihood per observation, log loss is
#' extracted automatically only from a \code{glm} with family
#' \code{"binomial"} fitted to ungrouped data (all prior weights equal to
#' one); for such a fit \code{logLoss(x)} equals \code{deviance(x) / (2 * n)}.
#' For every other model class, pass response and predicted probabilities
#' explicitly.
#'
#' @param x Either a numeric or logical vector containing the observed binary
#'   outcomes (0/1) when \code{pred} is supplied, or a fitted binomial
#'   \code{glm} from which response and fitted probabilities are extracted.
#'   Factors are not accepted, as the choice of the positive level would have
#'   to be guessed.
#' @param pred Numeric vector containing predicted probabilities in
#'   \eqn{[0,1]}. Required when \code{x} is a response vector.
#' @param eps Numeric scalar in \eqn{(0, 0.5)} used to clip probabilities
#'   away from 0 and 1. Defaults to \code{.Machine$double.eps}.
#'
#' @return A numeric scalar containing the log loss.
#'
#' @examples
#' resp <- c(0, 0, 1, 1)
#' pred <- c(0.1, 0.4, 0.35, 0.8)
#' logLoss(resp, pred)
#'
#' # a confidently wrong prediction is capped by eps
#' logLoss(c(0, 1), c(1, 0), eps = 1e-6)
#'
#' m <- glm(am ~ hp + wt, data = mtcars, family = binomial)
#' logLoss(m)
#'
#' # equivalently, half the mean deviance
#' m$deviance / (2 * nrow(mtcars))
#'
#' @family model.metrics
#' @concept model-evaluation
#' @concept calibration
#' @concept classification
#' @export
logLoss <- function(x, pred = NULL, eps = .Machine$double.eps) {

  fromModel <- is.null(pred)

  # --- extract resp / pred ---------------------------------------------
  if (fromModel) {

    if (!inherits(x, "glm"))
      stop("automatic extraction is currently supported only for 'glm' ",
           "models; supply response and predictions explicitly otherwise.")

    if (!identical(x$family$family, "binomial"))
      stop("logLoss() requires a model with family \"binomial\", got \"",
           x$family$family, "\".")

    if (any(x$prior.weights != 1))
      stop("logLoss() is defined for ungrouped Bernoulli data, but the ",
           "model has non-unit prior weights.")

    # $fitted.values and $y are both stored unpadded and are therefore
    # aligned even under na.action = na.exclude, where predict() pads
    pred <- x$fitted.values
    resp <- x$y

  } else {
    resp <- x
  }

  # --- validate resp / pred --------------------------------------------
  nmResp <- if (fromModel) "the model response" else "'x'"
  nmPred <- if (fromModel) "the fitted probabilities" else "'pred'"

  if (length(resp) != length(pred))
    stop(nmResp, " and ", nmPred, " must have the same length.")

  if (!length(resp))
    stop(nmResp, " and ", nmPred, " must not be empty.")

  if (!is.numeric(resp) && !is.logical(resp))
    stop(nmResp, " must be a numeric or logical vector of 0/1 values ",
         "(convert factors explicitly).")

  if (!is.numeric(pred))
    stop(nmPred, " must be numeric.")

  if (anyNA(resp) || anyNA(pred))
    stop(nmResp, " and ", nmPred, " must not contain missing values.")

  if (!all(resp %in% c(0L, 1L)))
    stop(nmResp, " must be binary (0/1).")

  if (any(!is.finite(pred)))
    stop(nmPred, " must contain finite values.")

  if (any(pred < 0 | pred > 1))
    stop(nmPred, " must contain probabilities in [0, 1].")

  if (!is.numeric(eps) || length(eps) != 1L || is.na(eps) ||
      eps <= 0 || eps >= 0.5)
    stop("'eps' must be a single numeric value in (0, 0.5).")

  # --- log loss ---------------------------------------------------------
  pred <- pmin(pmax(pred, eps), 1 - eps)

  -mean(resp * log(pred) + (1 - resp) * log1p(-pred))
}
