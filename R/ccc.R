
#' Lin's Concordance Correlation Coefficient
#'
#' Computes Lin's concordance correlation coefficient (CCC) for assessing
#' agreement between two continuous measurements.
#'
#' The CCC combines measures of precision and accuracy and quantifies the
#' deviation of the observed data from the line of perfect concordance.
#' Values range from -1 to 1, where 1 indicates perfect agreement.
#'
#' Confidence intervals can be computed using a Fisher z-transformation,
#' a nonparametric bootstrap, or the asymptotic approximation of
#' Lin (2000).
#'
#' Missing values are handled according to package conventions:
#' if \code{na.rm = FALSE} and either \code{x} or \code{y} contains missing
#' values, \code{NA} is returned. If \code{na.rm = TRUE}, complete cases are
#' used.
#'
#' @param x Numeric vector.
#' @param y Numeric vector of equal length to \code{x}.
#' @param conf.level Confidence level for the returned confidence interval.
#' Set to \code{NA} (default) to suppress confidence interval calculation.
#' @param sides Character string specifying a two-sided or one-sided
#' confidence interval.
#' @param method Character string specifying the confidence interval method.
#' One of \code{"z-transform"}, \code{"boot"}, or \code{"asymptotic"}.
#' @param na.rm Logical; if \code{TRUE}, incomplete observation pairs are
#' removed before computation.
#' @param ... Additional arguments passed to bootstrap procedures. For
#' \code{method = "boot"} these may include \code{R},
#' \code{parallel}, and related bootstrap controls.
#'
#' @return
#' A named numeric vector.
#'
#' If \code{conf.level = NA}:
#'
#' \preformatted{
#'       est
#' 0.9123457
#' }
#'
#' Otherwise:
#'
#' \preformatted{
#'       est       lci       uci
#' 0.9123457 0.8734211 0.9412837
#' }
#'
#' Additional diagnostics are stored as attributes:
#'
#' \describe{
#'   \item{nObs}{Number of observations used.}
#'   \item{scaleShift}{Scale shift parameter.}
#'   \item{locationShift}{Location shift parameter.}
#'   \item{biasCorrection}{Bias correction factor.}
#'   \item{method}{Confidence interval method (if applicable).}
#'   \item{conf.level}{Confidence level (if applicable).}
#'   \item{sides}{Confidence interval type (if applicable).}
#' }
#'
#' @references
#' Lin, L. I.-K. (1989). A concordance correlation coefficient to evaluate
#' reproducibility. \emph{Biometrics}, \emph{45}(1), 255-268.
#'
#' Lin, L. I.-K. (2000). A note on the concordance correlation coefficient.
#' \emph{Biometrics}, \emph{56}(1), 324-325.
#'
#' @examples
#' set.seed(123)
#'
#' x <- rnorm(100)
#' y <- x + rnorm(100, sd = 0.2)
#'
#' ccc(x, y)
#'
#' ccc(x, y, conf.level = 0.95)
#'
#' ccc(
#'   x, y,
#'   conf.level = 0.95,
#'   method = "boot",
#'   R = 999
#' )
#'
#' @family topic.association
#' @concept agreement
#' @concept concordance
#' @concept correlation
#'
#' @export
ccc <- function(
    x,
    y,
    conf.level = NA,
    sides = c("two.sided", "left", "right"),
    method = c("z-transform", "boot", "asymptotic"),
    na.rm = FALSE,
    ...
){
  
  if(!is.numeric(x))
    stop("Argument 'x' must be numeric.")
  
  if(!is.numeric(y))
    stop("Argument 'y' must be numeric.")
  
  if(length(x) != length(y))
    stop("Arguments 'x' and 'y' must have equal length.")
  
  sides <- match.arg(sides)
  method <- match.arg(method)
  
  if(!is.na(conf.level)) {
    
    if(!is.numeric(conf.level) ||
       length(conf.level) != 1L ||
       conf.level <= 0 ||
       conf.level >= 1) {
      
      stop(
        "Argument 'conf.level' must be a single number between 0 and 1."
      )
      
    }
    
  }
  
  if(na.rm) {
    
    keep <- complete.cases(x, y)
    
    x <- x[keep]
    y <- y[keep]
    
    if(length(x) < 3L) {
      
      stop(
        "After removing missing values, fewer than 3 observations remain."
      )
      
    }
    
  }
  
  if(anyNA(x) || anyNA(y))
    return(NA_real_)
  
  .cccEngine(
    x = x,
    y = y,
    conf.level = conf.level,
    sides = sides,
    method = method,
    ...
  )
  
}


.cccEngine <- function(
    x,
    y,
    conf.level,
    sides,
    method,
    ...
){
  
  nObs <- length(x)
  
  if(nObs < 3L)
    stop("At least 3 complete observations are required.")
  
  sx2 <- var(x) * (nObs - 1) / nObs
  sy2 <- var(y) * (nObs - 1) / nObs
  
  if(sx2 <= 0)
    stop("Argument 'x' must have positive variance.")
  
  if(sy2 <= 0)
    stop("Argument 'y' must have positive variance.")
  
  xb <- mean(x)
  yb <- mean(y)
  
  sdx <- sqrt(sx2)
  sdy <- sqrt(sy2)
  
  r <- cor(x, y)
  
  rhoC <- .cccPoint(x, y)
  
  geomMeanSd <- (sx2 * sy2)^0.25
  
  scaleShift <- sdy / sdx
  
  locationShift <-
    (yb - xb) / geomMeanSd
  
  if(abs(r) < sqrt(.Machine$double.eps)) {
    
    biasCorrection <- NA_real_
    
  } else {
    
    biasCorrection <- rhoC / r
    
  }
  
  attrs <- list(
    nObs = nObs,
    scaleShift = scaleShift,
    locationShift = locationShift,
    biasCorrection = biasCorrection
  )
  
  if(is.na(conf.level)) {
    
    return(
      .makeEstimateResult(
        est = rhoC,
        attrs = attrs
      )
    )
    
  }
  
  alpha <- 1 - conf.level
  
  if(method == "boot") {
    
    dots <- list(...)
    bootArgs <- .extractBootArgs(dots)
    
    statFun <- function(data, idx) {
      
      .cccPoint(
        data[idx, 1],
        data[idx, 2]
      )
      
    }
    
    bootObj <- boot::boot(
      data = cbind(x, y),
      statistic = statFun,
      R = bootArgs$R
    )
    
    probs <- switch(
      sides,
      "two.sided" = c(alpha / 2, 1 - alpha / 2),
      "left"      = c(0, conf.level),
      "right"     = c(1 - conf.level, 1)
    )
    
    ci <- quantile(
      bootObj$t,
      probs = probs,
      na.rm = TRUE
    )
    
    if(sides == "two.sided") {
      
      lci <- ci[1]
      uci <- ci[2]
      
    } else if(sides == "left") {
      
      lci <- -1
      uci <- ci[2]
      
    } else {
      
      lci <- ci[1]
      uci <- 1
      
    }
    
  } else {
    
    zCrit <- if(sides == "two.sided")
      qnorm(1 - alpha / 2)
    else
      qnorm(conf.level)
    
    se <- sqrt(
      (
        (1 - r^2) * rhoC^2 * (1 - rhoC^2) / r^2 +
          2 * rhoC^3 * (1 - rhoC) * locationShift^2 / r -
          0.5 * rhoC^4 * locationShift^4 / r^2
      ) / (nObs - 2)
    )
    
    if(method == "asymptotic") {
      
      if(sides == "two.sided") {
        
        lci <- rhoC - zCrit * se
        uci <- rhoC + zCrit * se
        
      } else if(sides == "left") {
        
        lci <- -1
        uci <- rhoC + zCrit * se
        
      } else {
        
        lci <- rhoC - zCrit * se
        uci <- 1
        
      }
      
      lci <- max(lci, -1)
      uci <- min(uci,  1)
      
    } else {
      
      # Avoid infinities in Fisher's z-transformation.
      rhoAdj <- pmin(
        pmax(rhoC, -1 + sqrt(.Machine$double.eps)),
        1 - sqrt(.Machine$double.eps)
      )
      
      z <- fisherZ(rhoAdj)
      
      # Delta-method variance transformation:
      # d atanh(rho) / d rho = 1 / (1 - rho^2)
      seZ <- se / (1 - rhoAdj^2)
      
      if(sides == "two.sided") {
        
        lci <- fisherZInv(
          z - zCrit * seZ
        )
        
        uci <- fisherZInv(
          z + zCrit * seZ
        )
        
      } else if(sides == "left") {
        
        lci <- -1
        
        uci <- fisherZInv(
          z + zCrit * seZ
        )
        
      } else {
        
        lci <- fisherZInv(
          z - zCrit * seZ
        )
        
        uci <- 1
        
      }
      
    }
    
  }
  
  attrs$method <- method
  attrs$conf.level <- conf.level
  attrs$sides <- sides
  
  .makeEstimateResult(
    est = rhoC,
    lci = lci,
    uci = uci,
    attrs = attrs
  )
  
}



.makeEstimateResult <- function(
    est,
    lci = NULL,
    uci = NULL,
    attrs = NULL
){
  
  res <- c(est = est)
  
  if(!is.null(lci))
    res <- c(res, lci = lci)
  
  if(!is.null(uci))
    res <- c(res, uci = uci)
  
  if(!is.null(attrs) && length(attrs))
    attributes(res) <- c(attributes(res), attrs)
  
  res
  
}


.cccPoint <- function(x, y){
  
  nObs <- length(x)
  sx2 <- var(x) * (nObs - 1) / nObs
  sy2 <- var(y) * (nObs - 1) / nObs
  r <- cor(x, y)
  sxy <- r * sqrt(sx2 * sy2)
  
  2 * sxy / (sx2 + sy2 + (mean(y) - mean(x))^2)
  
}

