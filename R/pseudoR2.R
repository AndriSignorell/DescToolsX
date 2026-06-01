
#' Pseudo R-squared Measures for Regression Models
#'
#' Computes a set of pseudo R-squared statistics for fitted regression models,
#' including generalized linear models and related extensions.
#'
#' The following measures are available:
#' \itemize{
#'   \item \strong{McFadden}: Likelihood ratio index.
#'   \item \strong{McFaddenAdj}: Adjusted McFadden index (penalized for model complexity).
#'   \item \strong{CoxSnell}: Maximum likelihood R-squared.
#'   \item \strong{Nagelkerke}: Scaled Cox-Snell R-squared.
#'   \item \strong{AldrichNelson}: Based on likelihood ratio.
#'   \item \strong{VeallZimmermann}: Correction of Aldrich-Nelson.
#'   \item \strong{McKelveyZavoina}: Latent variable R-squared (for logit/probit).
#'   \item \strong{Efron}: Squared correlation between observed and predicted values.
#'   \item \strong{Tjur}: Coefficient of discrimination (binomial models only).
#'   \item \strong{AIC}: Akaike Information Criterion.
#'   \item \strong{BIC}: Bayesian Information Criterion.
#'   \item \strong{logLik}: Log-likelihood of fitted model.
#'   \item \strong{logLik0}: Log-likelihood of null (intercept-only) model.
#'   \item \strong{G2}: Likelihood ratio statistic.
#' }
#' 
#' Supported classes include:
#' \itemize{
#'   \item \code{glm}
#'   \item \code{nnet::multinom} (requires package \pkg{nnet})
#'   \item \code{MASS::polr} (requires package \pkg{MASS})
#'   \item \code{VGAM::vglm} (requires package \pkg{VGAM})
#' }
#' 
#' @param fit A fitted model object. Supported classes include:
#' \itemize{
#'   \item \code{glm}
#'   \item \code{nnet::multinom}
#'   \item \code{MASS::polr}
#'   \item \code{VGAM::vglm} (requires package \pkg{VGAM})
#' }
#'
#' @param which Character vector specifying which statistic(s) to return.
#'   Defaults to \code{"McFadden"}. Use \code{"all"} to return all measures.
#'
#' @return A named numeric vector with the requested pseudo R-squared measure(s).
#'
#' @details
#' Pseudo R-squared measures provide analogues to the coefficient of determination
#' for models where ordinary R-squared is not defined, such as logistic or ordinal regression.
#'
#' The null model is internally refitted as an intercept-only model using
#' \code{model.frame(x)} to ensure consistency with the original data and handling
#' of missing values.
#'
#' For \code{vglm} objects, the package \pkg{VGAM} must be installed. The function
#' uses \code{VGAM::predictvglm()} for obtaining linear predictors.
#'
#' Some measures (e.g., McKelvey-Zavoina) depend on the link function and are only
#' defined for logit and probit links.
#'
#' @note
#' \itemize{
#'   \item Weighted models are supported only if weights are stored in the fitted model.
#'   \item Models fitted with aggregated responses (e.g., \code{cbind(success, failure)})
#'     may yield incorrect results.
#'   \item For \code{vglm} models, ensure the model was fitted with \code{model = TRUE}
#'     to guarantee correct extraction of the model frame.
#' }
#'
#' @references
#' McFadden, D. (1974). Conditional logit analysis of qualitative choice behavior.
#'
#' Cox, D. R., & Snell, E. J. (1989). Analysis of Binary Data.
#'
#' Nagelkerke, N. J. D. (1991). A note on a general definition of the coefficient of determination.
#'
#' Veall, M. R., & Zimmermann, K. F. (1996). Pseudo-R2 measures for some common limited dependent variable models.
#'
#' @examples
#' # Logistic regression example
#' data(mtcars)
#' mtcars$am <- factor(mtcars$am)
#'
#' fit <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#'
#' pseudoR2(fit)
#' pseudoR2(fit, which = "Nagelkerke")
#' pseudoR2(fit, which = "all")
#'



#' @family regression.utils
#' @concept regression
#' @concept classification-metrics
#' @concept descriptive-statistics
#'
#'
#' @export
pseudoR2 <- function(fit, which = "McFadden") {
  
  info <- .getModelInfo(fit)
  
  preds <- .getPredictions(fit, info)
  
  nullmod <- .getNullModel(fit, info)
  
  core <- .computeCoreMetrics(fit, nullmod, info)
  
  if(info$type == "glm") {
    extra <- .computeGLMMetrics(fit, preds, core, info)
    core[names(extra)] <- extra
  }
  
  if(identical(which, "all")) return(core)
  
  which <- match.arg(which, names(core), several.ok = TRUE)
  core[which]
}




# == internal helper functions =============================================


#' @keywords internal
#' @noRd
.isVglm <- function(x) {
  inherits(x, "vglm")
}



#' @keywords internal
#' @noRd
.getModelInfo <- function(x) {
  
  if(inherits(x, "glm")) {
    type <- "glm"
    
    L.full <- logLik(x)
    
    return(list(
      type = type,
      logLik = L.full,
      n = attr(L.full, "nobs"),
      edf = x$rank,
      AIC = AIC(x),
      BIC = BIC(x)
    ))
  }
  
  if(.isVglm(x)) {
    
    if(!requireNamespace("VGAM", quietly = TRUE)) {
      stop("Package 'VGAM' required for vglm models")
    }
    
    L.full <- logLik(x)
    
    return(list(
      type = "vglm",
      logLik = L.full,
      n = nobs(x),          # wichtig: logLik liefert das nicht!
      edf = x@rank,
      AIC = AIC(x),
      BIC = BIC(x)
    ))
  }
  
  if(inherits(x, "multinom")) {
    return(list(
      type = "multinom",
      logLik = logLik(x),
      n = attr(logLik(x), "nobs"),
      edf = x$edf,
      AIC = AIC(x),
      BIC = BIC(x)
    ))
  }
  
  if(inherits(x, "polr")) {
    return(list(
      type = "polr",
      logLik = logLik(x),
      n = attr(logLik(x), "nobs"),
      edf = x$rank,
      AIC = AIC(x),
      BIC = BIC(x)
    ))
  }
  
  stop("Unsupported model type")
}


#' @keywords internal
#' @noRd
.getPredictions <- function(x, info) {
  
  if(info$type == "glm") {
    return(list(
      link = predict(x, type = "link"),
      resp = predict(x, type = "response"),
      y = x$y,
      family = x$family$family,
      linkfun = x$family$link
    ))
  }
  
  if(info$type == "vglm") {
    
    link <- VGAM::predictvglm(x, type = "link")
    resp <- predict(x, type = "response")
    
    # family + link extrahieren
    fam <- x@family@vfamily
    
    linkfun <- if(all(x@misc$link == "logit")) {
      "logit"
    } else if(all(x@misc$link == "probit")) {
      "probit"
    } else {
      NA
    }
    
    return(list(
      link = link,
      resp = resp,
      y = x@y,
      family = fam,
      linkfun = linkfun
    ))
  }
  
  NULL
}


#' @keywords internal
#' @noRd
.getNullModel <- function(x, info) {
  
  f <- formula(x)
  yname <- all.vars(f)[1]
  
  data <- model.frame(x)
  
  null_formula <- as.formula(paste(yname, "~ 1"))
  
  if(info$type == "glm") {
    
    return(glm(null_formula,
               data = data,
               family = x$family,
               weights = x$prior.weights))
  }
  
  if(info$type == "vglm") {
    
    return(VGAM::vglm(
      formula = null_formula,
      data = data,
      family = x@family,
      weights = if(!is.null(x@prior.weights)) as.vector(x@prior.weights) else NULL,
      control = x@control
    ))
  }
  
  if(info$type == "multinom") {
    
    if(!requireNamespace("nnet", quietly = TRUE)) {
      stop("Package 'nnet' required for multinom models")
    }
    
    return(nnet::multinom(null_formula,
                          data = data,
                          weights = x$weights,
                          trace = FALSE))
  }
  
  if(info$type == "polr") {
    
    if(!requireNamespace("MASS", quietly = TRUE)) {
      stop("Package 'MASS' required for polr models")
    }
    
    return(MASS::polr(null_formula,
                      data = data,
                      weights = x$weights,
                      method = x$method))
  }
}



#' @keywords internal
#' @noRd
.computeGLMMetrics <- function(x, preds, core, info) {
  
  if(info$type %notin% c("glm", "vglm"))
    return(NULL)
  
  y <- preds$y
  yhat <- preds$link
  yresp <- preds$resp
  n <- info$n
  
  s2 <- switch(preds$linkfun,
               probit = 1,
               logit = pi^2 / 3,
               NA)
  
  out <- c(
    
    AldrichNelson = core["G2"] / (core["G2"] + n),
    
    VeallZimmermann =
      (core["G2"] / (core["G2"] + n)) *
      (2 * core["logLik0"] - n) / (2 * core["logLik0"]),
    
    McKelveyZavoina = {
      sse <- sum((yhat - mean(yhat))^2)
      sse / (n * s2 + sse)
    },
    
    Efron = 1 - sum((y - yresp)^2) / sum((y - mean(y))^2)
  )
  
  if(identical(preds$family, "binomial")) {
    out["Tjur"] <- diff(tapply(yresp, y, mean))
  }
  
  out
}



#' @keywords internal
#' @noRd
.computeCoreMetrics <- function(x, nullmod, info) {
  
  L.full <- info$logLik
  L.base <- logLik(nullmod)
  
  n <- info$n
  
  D.full <- -2 * L.full
  D.base <- -2 * L.base
  G2 <- -2 * (L.base - L.full)
  
  res <- c(
    
    McFadden = 1 - (L.full / L.base),
    
    McFaddenAdj = 1 - ((L.full - info$edf) / L.base),
    
    CoxSnell = 1 - exp(-G2 / n),
    
    Nagelkerke = (1 - exp((D.full - D.base) / n)) /
      (1 - exp(-D.base / n)),
    
    AIC = info$AIC,
    BIC = info$BIC,
    
    logLik = L.full,
    logLik0 = L.base,
    G2 = G2
  )
  
  res
}

