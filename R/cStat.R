
#' Concordance statistic (C-statistic / AUC)
#'
#' Computes the concordance statistic (C-statistic), equivalent to the
#' area under the ROC curve (AUC), for predicted values and a binary outcome.
#'
#' @param x An object for which the C-statistic should be computed, 
#' numeric vector of predicted values in vector interface.
#' @param resp the response of a model.
#' @param conf.level confidence level for CI, \code{NA} will return none.
#' @param ... Additional arguments passed to methods.
#'
#' @return A numeric value between 0 and 1 representing the C-statistic.
#'
#' @name cStat
#' 
#' @details
#' The C-statistic is defined as the probability that, for a randomly chosen
#' pair of observations with different outcomes, the observation with the
#' higher predicted value has the higher observed outcome.
#'
#' Ties in the outcome are handled by assigning a weight of 0.5.
#' 
#' This implementation uses:
#' \itemize{
#'   \item O(n log n) concordance computation
#'   \item Parallel bootstrap confidence intervals via \pkg{RcppParallel}
#'   \item Efficient memory handling
#' }
#' 
#' Number of bootstrap samples for the boostrap CIs can be given
#' in the dots as \code{R = 5,000} (default is 1,000). 
#' 
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
#'              data = bedrock::d.pizza, family = binomial)
#' cStat(r.mod, conf.level=0.95)
#'



#' @family confusion
#' @concept classification-metrics
#' @concept prediction-accuracy
#' @concept descriptive-statistics
#'
#'
#' @export
cStat <- function(x, ...) {
  UseMethod("cStat")
}


#' @method cStat glm
#' @rdname cStat
#' @export
#' @importFrom stats predict model.response
cStat.glm <- function(x, ...) {
  cStat.default(
    x = predict(x, type = "response"),
    resp = model.response(x$model),
    ...
  )
}


#' @method cStat default
#' @rdname cStat
#' @param resp A binary response vector (numeric, logical, or factor).
#' @export
cStat.default <- function(x, resp, conf.level=NA, ...) {
  
  if (missing(resp)) {
    stop("`resp` must be provided for the default method.", call. = FALSE)
  }
  
  # --- basic checks ---
  if (length(x) != length(resp)) {
    stop("`x` and `resp` must have the same length.", call. = FALSE)
  }
  
  # convert response to numeric {0,1}
  y <- as.numeric(factor(resp)) - 1
  
  if (!all(y %in% c(0, 1))) {
    stop("`resp` must be binary.", call. = FALSE)
  }
  
  # --- compute concordance ---
  z <- conDisPairsXY_ind_cpp(y, x)
  
  res <- (z["C"] + 0.5 * z["Ties_Y"]) / (z["D"] + z["C"] + z["Ties_Y"])
  
  if(!is.na(conf.level)){
    # get confidence intervals
    seed <- sample.int(.Machine$integer.max, 1)
    
    R <- list(...)$R %||% 1000 
    ci <- cstat_bootstrap_parallel_cpp(y, x, R, 
                                       alpha = 1-conf.level, 
                                       seed = seed)
    
    res <-  c(est=ci[1], lci=ci[2], uci=ci[3])
    
  }
  
  return(res)
  
}



# == internal helper functions ==========================================

.cStatCI <- function(x, resp, R = 1000, conf.level = 0.95) {

  if (length(x) != length(resp)) {
    stop("`x` and `resp` must have same length.", call. = FALSE)
  }
  
  y <- as.numeric(factor(resp)) - 1
  
  if (!all(y %in% c(0, 1))) {
    stop("`resp` must be binary.", call. = FALSE)
  }
  
  cstat_bootstrap_parallel_cpp(x, y, R, alpha = 1-conf.level)
  
}


