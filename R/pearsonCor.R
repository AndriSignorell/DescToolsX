
#' Confidence Intervals for Pearson Correlation
#' 
#' Find the confidence intervals for a specified correlation based on Fisher's
#' z-transformation.
#' 
#' The sampling distribution of Pearson's r is not normal. Fisher developed a
#' transformation now called "Fisher's z-transformation" used for the
#' calculation of normal distributed confidence intervals.
#' 
#' @param x a numeric vector, matrix, or table
#' @param y `NULL` (default) or a vector with compatible dimensions to
#' `x`. If `y` is supplied, `table(x, y, ...)` is calculated.

#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()].
#'   
#' @param scoresType score calculation method for table input
#' @param na.rm logical, default `FALSE` determining if complete cases
#' should be respected
#' @return if `conf.level = NA`, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{`est`}{point estimate of Pearson's correlation coefficient}
#'   \item{`lci`}{lower confidence interval bound}
#'   \item{`uci`}{upper confidence interval bound}
#' }
#' 
#' @note Based on code by William Revelle, adapted to conform to package standards.
#' 
#' 
#' @examples
#' 
#' with(swiss, pearsonCor(Fertility, Agriculture))
#' with(swiss, pearsonCor(Fertility, Agriculture, conf.level=0.95))
#' 
#' bedrock::pairApply(swiss, pearsonCor)
#' 
#' bedrock::pairApply(swiss, 
#'            function(x, y) fmCI(pearsonCor(x, y, conf.level=0.95), 
#'                                digits=3, leadDigits=0))
#' 
#' @seealso [lumen::fisherZ], [lumen::fisherZInv]
#' 
#' @family assoc.continuous  
#' @concept correlation  
#' @concept association-measure
#'
#'
#' @export
pearsonCor <- function(x, y = NULL,
                       conf.level = NA,
                       sides = c("two.sided","left","right"),
                       scoresType = "table",
                       na.rm = FALSE) {
  
  sides  <- match.arg(sides)

  
  # --------------------------------------------------
  # Compute correlation
  # --------------------------------------------------
  
  if (is.null(y)) {
    
    # Table interface
    sR <- scores(x, 1, scoresType)
    sC <- scores(x, 2, scoresType)
    
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
      n <- sum(!is.na(x) & !is.na(y))

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

  # NULL breaks the est/lci/uci contract that every other exit of this
  # function keeps - pearsonCor(c(1, NA), c(2, 3), conf.level = 0.95)
  # returned NULL rather than a named triple, and .assocsTab() consumes
  # the result positionally.
  if (is.na(conf.level) || is.na(r))
    return(c(est = NA_real_, lci = NA_real_, uci = NA_real_))

  if (n < 4)
    return(c(est = r, lci = NA_real_, uci = NA_real_))
  
  if ( isZero(abs(r) - 1) )
    return(c(est = r, lci = r, uci = r))
  
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



