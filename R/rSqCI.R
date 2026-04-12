
#' Bootstrap Confidence Interval for R-squared of a Linear Model
#'
#' Computes a bootstrap confidence interval for the (adjusted) R-squared
#' of a linear model fitted via \code{\link[stats]{lm}} using a fast
#' parallel \pkg{RcppParallel} implementation.
#'
#' The bootstrap is based on resampling observations (pairs bootstrap).
#'
#' @param x An object of class \code{"lm"}.
#' @param conf.level Confidence level for the interval. Default is \code{0.95}.
#' @param sides A character string specifying the type of interval:
#'   \code{"two.sided"} (default), \code{"left"}, or \code{"right"}.
#' @param adjusted Logical; if \code{TRUE} (default), the adjusted R-squared
#'   is used. Otherwise, the ordinary R-squared is computed.
#' @param B Number of bootstrap replicates. Default is \code{2000}.
#' @param seed Optional integer seed for reproducibility. If \code{NULL}
#'   (default), a random seed is used.
#' @param ... Further arguments (currently unused).
#'
#' @details
#' The function performs a nonparametric bootstrap by resampling the rows
#' of the model matrix and response vector. For each bootstrap sample,
#' the linear model is refitted and the R-squared statistic is computed.
#'
#' The confidence interval is constructed using the empirical quantiles
#' of the bootstrap distribution.
#'
#' For adjusted R-squared, the following formula is used:
#' \deqn{
#' R^2_{adj} = 1 - (1 - R^2)\frac{n - 1}{n - p - 1}
#' }
#' where \eqn{p} is the number of predictors (excluding the intercept).
#'
#' Degenerate bootstrap samples (e.g., due to singular design matrices)
#' are discarded.
#'
#' @return A named numeric vector with elements:
#' \describe{
#'   \item{est}{Point estimate of (adjusted) R-squared from the original model.}
#'   \item{lci}{Lower confidence limit.}
#'   \item{uci}{Upper confidence limit.}
#' }
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#'
#' rSqCI(fit)
#'
#' rSqCI(fit, adjusted = FALSE, B = 1000, seed = 123)
#'
#' rSqCI(fit, sides = "left")
#'
#' @seealso \code{\link[stats]{lm}}, \code{\link[stats]{summary.lm}}
#'


#' @export
rSqCI <- function(x,
                  conf.level = 0.95,
                  sides = c("two.sided", "left", "right"),
                  adjusted = TRUE,
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
  
  res <- rsq_bootstrap_parallel_cpp(
    X, y,
    B = B,
    alpha = alpha,
    adjusted = adjusted,
    seed = ifelse(is.null(seed), -1L, seed)
  )
  
  if (sides == "left") res["uci"] <- NA
  if (sides == "right") res["lci"] <- NA
  
  res
}

