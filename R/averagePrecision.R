#' Average Precision Score
#'
#' Computes average precision (AP) for binary probabilistic predictions.
#'
#' @details
#' Average precision summarizes the precision-recall curve as
#'
#' \deqn{AP = \sum_n (R_n - R_{n-1}) P_n,}
#'
#' where \eqn{P_n} and \eqn{R_n} are precision and recall at the
#' \eqn{n}-th distinct prediction threshold. Unlike metrics based on
#' predicted class labels, average precision does not require a classification
#' cutoff. Prediction values therefore need only be numeric scores; they are
#' not restricted to the interval \[0, 1\].
#'
#' @param x either a numeric vector of observed binary outcomes (0/1) when
#'   \code{pred} is supplied, or a fitted model object from which both response
#'   and predictions are extracted
#' @param pred numeric vector of predicted probabilities or scores. Required
#'   when \code{x} is a response vector; ignored when \code{x} is a model
#'   object.
#'
#' @return A numeric scalar containing the average precision score.
#'
#' @examples
#' resp <- c(0, 0, 1, 1)
#' pred <- c(0.1, 0.4, 0.35, 0.8)
#' averagePrecision(resp, pred)
#'
#' @family model.metrics
#' @concept model-evaluation
#' @concept classification
#' @concept precision-recall
#' @export
averagePrecision <- function(x, pred = NULL) {

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
  if (!is.numeric(pred))
    stop("'pred' must be numeric.")
  if (!length(resp))
    stop("'x' and 'pred' must not be empty.")
  if (!any(resp == 1L))
    stop("'x' must contain at least one positive observation.")

  # --- average precision -----------------------------------------------
  ord  <- order(pred, decreasing = TRUE)
  resp <- resp[ord]
  pred <- pred[ord]

  # Tied scores form one threshold. Evaluating only after the final
  # observation in each tie group makes AP independent of tie ordering.
  idx <- c(which(diff(pred) != 0), length(pred))

  tp <- cumsum(resp)[idx]
  fp <- idx - tp

  precision <- tp / (tp + fp)
  recall    <- tp / sum(resp)

  sum(diff(c(0, recall)) * precision)
}
