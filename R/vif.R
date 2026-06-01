
#' Variance Inflation Factors (VIF / GVIF)
#'
#' Computes variance inflation factors (VIF) or generalized VIFs (GVIF)
#' for a fitted model. For multi-parameter terms (e.g., factors),
#' GVIFs are returned along with a scaled version \code{GVIF^(1/(2*Df))}.
#'
#' @param fit A fitted model object. Currently supports objects of class
#'   \code{lm}, \code{glm}, and \code{gls}.
#'
#' @return
#' If all terms have 1 degree of freedom, a named numeric vector of VIFs.
#' Otherwise, a matrix with columns:
#' \describe{
#'   \item{GVIF}{Generalized variance inflation factor}
#'   \item{Df}{Degrees of freedom for the term}
#'   \item{GVIF^(1/(2*Df))}{Scaled GVIF for comparability}
#' }
#'
#' @details
#' The function is based on the implementation in the \pkg{car} package
#' (Fox and Weisberg). GVIFs are computed from the correlation matrix
#' of the model coefficients.
#'
#' Interpretation:
#' \itemize{
#'   \item Values close to 1 indicate low multicollinearity
#'   \item Values > 5 or 10 may indicate problematic collinearity
#' }
#'
#' Note that VIFs are only meaningful if the model includes an intercept.
#' For models without intercept, a warning is issued.
#'
#' @examples
#' mod <- lm(mpg ~ wt + cyl, data = mtcars)
#' vif(mod)
#'

#' @family regression.utils
#' @concept regression
#' @concept data-inspection
#'
#'
#' @export
vif <- function(fit) {
  
  if (!inherits(fit, c("lm", "glm", "gls"))) {
    stop("Unsupported model type.", call. = FALSE)
  }
  
  if (any(is.na(coef(fit))))
    stop("There are aliased coefficients in the model.", call. = FALSE)
  
  v <- vcov(fit)
  mm <- model.matrix(fit)
  assign <- attr(mm, "assign")
  
  if (names(coef(fit))[1] == "(Intercept)") {
    v <- v[-1, -1, drop = FALSE]
    assign <- assign[-1]
  } else {
    warning("No intercept: VIFs may not be sensible.")
  }
  
  terms <- attr(terms(fit), "term.labels")
  n.terms <- length(terms)
  
  if (n.terms < 2)
    stop("Model contains fewer than 2 terms.", call. = FALSE)
  
  R <- cov2cor(v)
  log_detR <- determinant(R, logarithm = TRUE)$modulus
  
  result <- matrix(NA_real_, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
  
  for (term in seq_len(n.terms)) {
    subs <- which(assign == term)
    
    log_det_sub <- determinant(R[subs, subs, drop = FALSE], TRUE)$modulus
    log_det_rest <- determinant(R[-subs, -subs, drop = FALSE], TRUE)$modulus
    
    result[term, 1] <- exp(log_det_sub + log_det_rest - log_detR)
    result[term, 2] <- length(subs)
  }
  
  if (all(result[, 2] == 1)) {
    result <- result[, 1]
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  
  result
}



#' Model matrix for gls objects
#'
#' Extracts the model matrix from a \code{gls} object by reconstructing it
#' from the model formula and data.
#'
#' @param object A fitted \code{gls} model object (from \pkg{nlme}).
#' @param ... Additional arguments (currently ignored).
#'
#' @return A model matrix corresponding to the fixed-effects design matrix.
#'
#' @details
#' This function provides an S3 method for \code{model.matrix()} for
#' \code{gls} objects, which do not have a built-in method in base R.
#'
#' The data is extracted from the original model call. This may fail if
#' the data is not available in the evaluation environment.
#'
#' @examples
#' \dontrun{
#' library(nlme)
#' mod <- gls(distance ~ age, data = Orthodont)
#' model.matrix(mod)
#' }
#'
#' @method model.matrix gls
#' @export
model.matrix.gls <- function(object, ...) {
  data <- tryCatch(
    eval(object$call$data, envir = parent.frame()),
    error = function(e)
      stop("Could not evaluate model data.", call. = FALSE)
  )
  
  model.matrix(formula(object), data = data)
}