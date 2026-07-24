
#' Concordance Statistic (C-Statistic / AUC)
#'
#' Computes the concordance statistic (C-statistic), equivalent to the
#' area under the ROC curve (AUC), for predicted values and a binary outcome.
#'
#' @param x an object for which the C-statistic should be computed; for the
#' default method, a numeric vector of predicted values
#' @param resp binary response vector
#' @param conf.level confidence level for the interval; \code{NA} suppresses
#' interval calculation
#' @param ... additional arguments passed to methods
#'
#' @return if \code{conf.level = NA}, a numeric scalar between 0 and 1;
#' otherwise a named numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of the C-statistic.}
#'   \item{\code{lci}}{lower confidence interval bound.}
#'   \item{\code{uci}}{upper confidence interval bound.}
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
#' This implementation uses:
#' \itemize{
#'   \item O(n log n) concordance computation
#'   \item Parallel bootstrap confidence intervals via \pkg{RcppParallel}
#'   \item Efficient memory handling
#' }
#' 
#' The number of bootstrap samples can be supplied through \code{...} as
#' \code{R}; the default is 1000.
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
#'              data = bedrock::Pizza, family = binomial)
#' cStat(r.mod, conf.level=0.95)
#'




#' @family assoc.ordinal  
#' @concept association-measure  
#' @concept ordinal  
#' @concept roc
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
#' @param resp a binary response vector (numeric, logical, or factor)
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
  z <- condis_pairs_xy_cpp(y, x)
  
  res <- (z["C"] + 0.5 * z["Ties_Y"]) / (z["D"] + z["C"] + z["Ties_Y"])
  
  if(!is.na(conf.level)){
    # get confidence intervals
    seed <- sample.int(.Machine$integer.max, 1)
    
    R <- list(...)$R %||% 1000 
    ci <- cstat_boot_cpp(y, x, R, 
                                       alpha = 1-conf.level, 
                                       seed = seed)
    
    res <-  c(est=ci[1], lci=ci[2], uci=ci[3])
    
  }
  
  return(res)
  
}
