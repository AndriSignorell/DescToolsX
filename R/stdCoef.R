
#' Standardized Regression Coefficients
#'
#' Computes standardized regression coefficients for a fitted model object.
#' Optionally, a *partial standardization* can be applied that adjusts for
#' multicollinearity using variance inflation factors (VIF).
#'
#' @description
#' This function rescales regression coefficients and their standard errors
#' by the ratio of standard deviations of predictors and outcome.
#'
#' If \code{partial_sd = TRUE}, a partial standardization is used:
#' the scaling additionally accounts for multicollinearity via VIF and
#' adjusts for the number of predictors and sample size.
#'
#' @param x A fitted model object (e.g. from \code{lm()} or \code{glm()}).
#' @param partial_sd Logical. If \code{TRUE}, partial standardization is applied.
#' @param ... Additional arguments passed to \code{coefTable()}.
#'
#' @details
#' Standard (full) standardization rescales coefficients as:
#' \deqn{
#' \beta^* = \beta \cdot \frac{\mathrm{sd}(x_j)}{\mathrm{sd}(y)}
#' }
#'
#' Partial standardization rescales coefficients as:
#' \deqn{
#' \beta^* = \beta \cdot \mathrm{sd}(x_j) \cdot \sqrt{\frac{1}{\mathrm{VIF}_j}} \cdot \sqrt{\frac{n - 1}{n - p}}
#' }
#'
#' where \eqn{\mathrm{VIF}_j} is the variance inflation factor of predictor \eqn{j},
#' \eqn{n} is the number of observations, and \eqn{p} is the number of predictors.
#'
#' The intercept is not standardized and will return \code{NA}.
#'
#' @return
#' A coefficient table (matrix) with standardized estimates and standard errors.
#' The first two columns are renamed to \code{"Estimate*"} and \code{"Std. Error*"}.
#'
#' @seealso
#' \code{\link{lm}}, \code{\link{glm}}, \code{\link[car]{vif}}
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#'
#' # Standardized coefficients
#' stdCoef(fit)
#'
#' # Partial standardized coefficients (adjusted for multicollinearity)
#' stdCoef(fit, partial_sd = TRUE)
#'



#' #' @export
#' stdCoef <- function(x, partial_sd = FALSE, ...) {
#'   
#'   coefmat <- coefTable(x, ...)
#'   mf <- model.frame(x)
#'   mm <- model.matrix(x)
#'   
#'   sx <- apply(mm, 2, sd, na.rm = TRUE)
#'   
#'   if (partial_sd) {
#'     bx <- .partialSD(x)
#'     
#'   } else {
#'     sy <- sd(model.response(mf), na.rm = TRUE)
#'     
#'     is_intercept <- names(sx) == "(Intercept)"
#'     sx[is_intercept] <- NA
#'     
#'     bx <- sx / sy
#'   }
#'   
#'   # Alignment (decisive!)
#'   bx <- bx[match(rownames(coefmat), names(bx))]
#'   
#'   coefmat[, 1:2] <- coefmat[, 1:2] * bx
#'   colnames(coefmat)[1:2] <- c("Estimate*", "Std. Error*")
#'   
#'   coefmat
#'   
#' }



# == internal helper functions ================================================


.partialSD <- function(x) {
  
  mm <- model.matrix(x)
  
  sx  <- apply(mm, 2, sd, na.rm = TRUE)
  vif <- vif(x)
  n   <- nobs(x)
  p   <- sum(attr(mm, "assign") != 0)
  
  # remove intercept 
  is_intercept <- names(sx) == "(Intercept)"
  sx[is_intercept]  <- NA
  vif[is_intercept] <- NA
  
  # alignment
  common <- intersect(names(sx), names(vif))
  sx  <- sx[common]
  vif <- vif[common]
  
  sx * sqrt(1 / vif) * sqrt((n - 1) / (n - p))
  
}

