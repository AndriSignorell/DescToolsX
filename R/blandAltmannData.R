
#' Bland-Altman Agreement Data
#'
#' Computes the quantities required for a Bland-Altman agreement analysis.
#'
#' For each observation pair, the arithmetic mean and the difference
#' (\code{y - x}) are calculated. The function further computes the mean
#' difference (bias), limits of agreement (LoA), and approximate confidence
#' intervals for the bias and LoA according to Bland and Altman.
#'
#' The returned object is of class \code{"blandAltman"} and can be plotted
#' using \code{plot()} when the Aurora package is installed.
#'
#' @name blandAltmanData
#' @param x numeric vector or formula
#' @param y numeric vector
#' @param data optional data frame used with the formula interface
#' @param conf.level confidence level
#' @param na.rm logical; if \code{TRUE}, incomplete observation pairs are
#' removed before computation
#' @param ... further arguments passed to or from other methods
#' 
#' @return
#' an object of class \code{"BlandAltman"} with components:
#'
#' \describe{
#'   \item{\code{mean}}{pairwise means}
#'   \item{\code{diff}}{pairwise differences (\code{y - x})}
#'   \item{\code{bias}}{mean difference}
#'   \item{\code{loaLower}}{lower limit of agreement}
#'   \item{\code{loaUpper}}{upper limit of agreement}
#'   \item{\code{biasCI}}{confidence interval for the bias}
#'   \item{\code{loaLowerCI}}{confidence interval for the lower LoA}
#'   \item{\code{loaUpperCI}}{confidence interval for the upper LoA}
#'   \item{\code{nObs}}{number of observations used}
#' }
#'
#' @references
#' Bland JM, Altman DG (1986).
#' Statistical methods for assessing agreement between two methods of
#' clinical measurement.
#' \emph{Lancet}, 327, 307-310.
#'
#' @examples
#' x <- rnorm(100)
#' y <- x + rnorm(100, sd = 0.5)
#'
#' ba <- blandAltmanData(x, y)
#'
#' @rdname blandAltmanData

#' @family assoc.agreement  
#' @concept agreement  
#' @concept method-comparison
#'
#'
#' @export
blandAltmanData <- function(
    x,
    ...
){
  UseMethod("blandAltmanData")
}


#' @rdname blandAltmanData
#' @export
blandAltmanData.default <- function(
    x,
    y,
    conf.level = 0.95,
    na.rm = FALSE,
    ...
){
  
  if(!is.numeric(x))
    stop("Argument 'x' must be numeric.")
  
  if(!is.numeric(y))
    stop("Argument 'y' must be numeric.")
  
  if(length(x) != length(y))
    stop("Arguments 'x' and 'y' must have equal length.")
  
  if(na.rm){
    
    keep <- complete.cases(x, y)
    
    x <- x[keep]
    y <- y[keep]
    
  }
  
  if(anyNA(x) || anyNA(y))
    return(NA)
  
  .blandAltmanData(
    x = x,
    y = y,
    conf.level = conf.level
  )
  
}


#' @rdname blandAltmanData
#' @export
blandAltmanData.formula <- function(
    x,
    data = NULL,
    conf.level = 0.95,
    na.rm = FALSE,
    ...
){
  
  z <- bedrock::resolveFormula(
    formula = x,
    data = data
  )
  
  blandAltmanData(
    x = z$x,
    y = z$y,
    conf.level = conf.level,
    na.rm = na.rm
  )
  
}


.blandAltmanData <- function(
    x,
    y,
    conf.level = 0.95
){
  
  nObs <- length(x)
  
  if(nObs < 3L)
    stop("At least 3 complete observations are required.")
  
  meanVals <- (x + y) / 2
  diffVals <- y - x
  
  bias <- mean(diffVals)
  
  sdDiff <- sd(diffVals)
  
  loaLower <- bias - 1.96 * sdDiff
  loaUpper <- bias + 1.96 * sdDiff
  
  alpha <- 1 - conf.level
  zCrit <- qnorm(1 - alpha / 2)
  
  seBias <- sdDiff / sqrt(nObs)
  
  seLoA <- sqrt(
    3 * sdDiff^2 / nObs
  )
  
  biasCI <- c(
    bias - zCrit * seBias,
    bias + zCrit * seBias
  )
  
  loaLowerCI <- c(
    loaLower - zCrit * seLoA,
    loaLower + zCrit * seLoA
  )
  
  loaUpperCI <- c(
    loaUpper - zCrit * seLoA,
    loaUpper + zCrit * seLoA
  )
  
  structure(
    
    list(
      
      mean = meanVals,
      diff = diffVals,
      
      bias = bias,
      
      loaLower = loaLower,
      loaUpper = loaUpper,
      
      biasCI = biasCI,
      
      loaLowerCI = loaLowerCI,
      loaUpperCI = loaUpperCI,
      
      nObs = nObs
      
    ),
    
    class = "BlandAltman"
    
  )
  
}



#' Print a Bland-Altman Analysis
#'
#' Prints a compact summary of a Bland-Altman agreement analysis including
#' the estimated bias, limits of agreement, corresponding confidence
#' intervals, and the number of observations.
#'
#' @param x an object of class \code{"BlandAltman"} as returned by
#' \code{\link{blandAltmanData}}
#' @param digits number of digits to display
#' @param ... further arguments passed to or from other methods
#'
#' @return
#' invisibly, \code{x}
#'
#'

#' @method print BlandAltman
#' @export
print.BlandAltman <- function(
    x,
    digits = getOption("digits"),
    ...
){
  
  cat("\n")
  cat("Bland-Altman Agreement Analysis\n")
  cat("\n")
  
  cat(
    sprintf(
      "Bias      : %.*f (%.*f, %.*f)\n",
      digits, x$bias,
      digits, x$biasCI[1],
      digits, x$biasCI[2]
    )
  )
  
  cat(
    sprintf(
      "Lower LoA : %.*f (%.*f, %.*f)\n",
      digits, x$loaLower,
      digits, x$loaLowerCI[1],
      digits, x$loaLowerCI[2]
    )
  )
  
  cat(
    sprintf(
      "Upper LoA : %.*f (%.*f, %.*f)\n",
      digits, x$loaUpper,
      digits, x$loaUpperCI[1],
      digits, x$loaUpperCI[2]
    )
  )
  
  cat("\n")
  
  cat(
    sprintf(
      "n = %d\n",
      x$nObs
    )
  )
  
  invisible(x)
  
}
