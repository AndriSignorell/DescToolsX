
#' Bootstrap Confidence Intervals for Linear Model Coefficients
#'
#' Computes bootstrap confidence intervals for regression coefficients
#' from a linear model using a fast parallel implementation.
#'
#' @param x An object of class \code{"lm"}.
#' @param conf.level Confidence level. Default is \code{0.95}.
#' @param sides Type of interval: \code{"two.sided"}, \code{"left"}, or \code{"right"}.
#' @param B Number of bootstrap samples.
#' @param seed Optional random seed.
#' @param ... Further arguments (unused).
#'
#' @details
#' Uses a nonparametric bootstrap (resampling observations).
#' For each sample, the linear model is refitted and coefficients are stored.
#'
#' Confidence intervals are based on empirical quantiles.
#'
#' @return A matrix with rows corresponding to coefficients and columns:
#' \code{est}, \code{lci}, \code{uci}.
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' coefCI(fit)
#'

#' @export
coefCI <- function(x,
                   conf.level = 0.95,
                   sides = c("two.sided", "left", "right"),
                   B = 2000,
                   seed = NULL,
                   ...) {
  
  if (!inherits(x, "lm"))
    stop("x must be an lm object")
  
  sides <- match.arg(sides)
  
  X <- model.matrix(x)
  y <- model.response(model.frame(x))
  
  alpha <- 1 - conf.level
  
  if (sides == "left") alpha <- 2 * alpha
  if (sides == "right") alpha <- 2 * alpha
  
  res <- coef_bootstrap_parallel_cpp(
    X, y,
    B = B,
    alpha = alpha,
    seed = ifelse(is.null(seed), -1L, seed)
  )
  
  rownames(res) <- colnames(X)
  
  if (sides == "left") res[, "uci"] <- NA
  if (sides == "right") res[, "lci"] <- NA
  
  res
}

