
#' Confidence Intervals for Pearson Correlation
#' 
#' Find the confidence intervals for a specified correlation based on Fisher's
#' z-transformation.
#' 
#' The sampling distribution of Pearson's r is not normal. Fisher developed a
#' transformation now called "Fisher's z-transformation" used for the
#' calculation of normal distributed confidence intervals.
#' 
#' @inheritParams ConfidenceIntervals
#' @param x a numeric vector or a 2x2 numeric matrix, resp. table.
#' @param y \code{NULL} (default) or a vector with compatible dimensions to
#' \code{x}. If y is provided, \code{table(x, y, ...)} will be calculated.
#' @param method method to calculate confidence intervals, implemented is 
#'         only "fisher". 
#' @param scores.type type of calculating scores for the table.
#' @param na.rm logical, default \code{FALSE} determining if complete cases
#' should be respected
#' @return est, lower and upper confidence intervals
#' @author William Revelle
#' \href{mailto:revelle@@northwestern.edu}{revelle@@northwestern.edu}, \cr
#' slight modifications Andri Signorell
#' \href{mailto:andri@@signorell.net}{andri@@signorell.net} based on R-Core
#' code
#' @seealso \code{\link{fisherZ}}, \code{\link{fisherZInv}}
#' 
#' @family correlation
#' @concept correlation
#' @concept continuous-data
#' @concept parametric
#' 
#' @examples
#' 
#' with(swiss, pearsonCor(Fertility, Agriculture))
#' with(swiss, pearsonCor(Fertility, Agriculture, conf.level=0.95))
#' 
#' pairApply(swiss, pearsonCor)
#' 
#' pairApply(swiss, 
#'           function(x, y) fmCI(pearsonCor(x, y, conf.level=0.95), 
#'                               digits=3, ldigits=0))
#' 


#' @export
pearsonCor <- function(x, y = NULL,
                       conf.level = NA,
                       sides = c("two.sided","left","right"),
                       method = c("fisher"),
                       scores.type = "table",
                       na.rm = FALSE) {
  
  sides  <- match.arg(sides)
  method <- match.arg(method)
  
  
  # --------------------------------------------------
  # Compute correlation
  # --------------------------------------------------
  
  if (is.null(y)) {
    
    # Table interface
    sR <- .scores(x, 1, scores.type)
    sC <- .scores(x, 2, scores.type)
    
    n  <- sum(x)
    
    Rbar <- sum(rowSums(x) * sR) / n
    Cbar <- sum(colSums(x) * sC) / n
    
    ssr <- sum(x * (sR - Rbar)^2)
    ssc <- sum(t(x) * (sC - Cbar)^2)
    
    tmpij <- outer(sR, sC,
                   FUN = function(a,b) (a - Rbar)*(b - Cbar))
    
    ssrc <- sum(x * tmpij)
    
    r <- ssrc / sqrt(ssr * ssc)
    
  } else {
    
    # Vector interface
    if (!na.rm && (anyNA(x) || anyNA(y))) {
      r <- NA_real_
      n <- sum(!is.na(x) & !is.na(y))  # still compute n for completeness
    
    } else {
      
      if (na.rm) {
        ok <- complete.cases(x, y)
        x  <- x[ok]
        y  <- y[ok]
      }
      
      n <- length(x)
      r <- cor(x, y)
      
    }
  }
  
  # --------------------------------------------------
  # Assemble result
  # --------------------------------------------------
  
  
  if(is.na(conf.level))
    res <- r
  else
    res <- .pearsonCI(r, n, conf.level, sides)

  return(res)
  
}



# --------------------------------------------------
# Internal CI helper
# --------------------------------------------------

.pearsonCI <- function(r, n, conf.level, sides) {
  
  if (is.na(conf.level) || is.na(r))
    return(NULL)
  
  if (n < 4)
    return(c(lwr.ci = NA_real_, upr.ci = NA_real_))
  
  if ( isZero(abs(r) - 1) )
    return( c(est=1, lci=1, uci=1) )
  
  alpha <- 1 - conf.level
  z  <- atanh(r)
  se <- 1 / sqrt(n - 3)
  
  if (sides == "two.sided") {
    zcrit <- qnorm(1 - alpha/2)
    ci_z  <- z + c(-1, 1) * zcrit * se
    
  } else if (sides == "left") {
    zcrit <- qnorm(conf.level)
    ci_z  <- c(z - zcrit * se, Inf)
    
  } else {
    zcrit <- qnorm(conf.level)
    ci_z  <- c(-Inf, z + zcrit * se)
  }
  
  ci <- tanh(ci_z)
  
  return( c(est=r, lci=ci[1], uci=ci[2]) )
  
}



# old TablePearson
# # 
# corX <- function (d, w = rep(1, nrow(d))/nrow(d)) {
# 
#   # table pearson correlation, taken for boot::corr
#   s <- sum(w)
#   m1 <- sum(d[, 1L] * w)/s
#   m2 <- sum(d[, 2L] * w)/s
#   (sum(d[, 1L] * d[, 2L] * w)/s - m1 * m2) /
#     sqrt((sum(d[, 1L]^2 * w)/s - m1^2) *
#            (sum(d[, 2L]^2 * w)/s - m2^2))
# }





