
#' Bland-Altman Agreement Data
#'
#' Computes the quantities required for a Bland-Altman agreement analysis.
#'
#' For each observation pair, the arithmetic mean and the difference
#' (\code{y - x}) are calculated. The function further computes the mean
#' difference (bias), limits of agreement (LoA), and approximate confidence
#' intervals for the bias and LoA according to Bland and Altman.
#'
#' The returned object is of class \code{"BlandAltman"} and can be plotted
#' using \code{plot()} when the \pkg{pharos} package is installed.
#'
#' The limits of agreement are the conventional \code{bias +/- 1.96 * sd},
#' independent of \code{conf.level}: the multiplier fixes the nominal
#' coverage of the interval of \emph{differences}, whereas \code{conf.level}
#' governs the confidence intervals reported for the bias and for the two
#' limits. The standard error of a limit uses the approximation
#' \eqn{\sqrt{3 s_d^2 / n}} given by Bland and Altman.
#'
#' @name blandAltmanData
#' @param x numeric vector or formula
#' @param y numeric vector
#' @param data optional data frame used with the formula interface
#' @param conf.level confidence level for the intervals reported for the
#'   bias and the limits of agreement
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
#'   \item{\code{conf.level}}{the confidence level used}
#' }
#'
#' @references
#' Bland JM, Altman DG (1986).
#' Statistical methods for assessing agreement between two methods of
#' clinical measurement.
#' \emph{Lancet}, 327, 307-310.
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#' y <- x + rnorm(100, sd = 0.5)
#'
#' ba <- blandAltmanData(x, y)
#' ba
#'
#' @rdname blandAltmanData
#' @family assoc.agreement
#' @concept agreement
#' @concept method-comparison
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

  if(!is.numeric(conf.level) || length(conf.level) != 1L ||
     is.na(conf.level) || conf.level <= 0 || conf.level >= 1)
    stop("Argument 'conf.level' must be a single number in (0, 1).")

  if(na.rm){

    keep <- complete.cases(x, y)

    x <- x[keep]
    y <- y[keep]

  }

  # Returning a bare logical NA here would hand back an object that is
  # neither of class "BlandAltman" nor plottable nor printable, and the
  # failure would surface far away from its cause. Every other invalid
  # input in this function stops, so this one does too.
  if(anyNA(x) || anyNA(y))
    stop("Missing values present; use na.rm = TRUE to drop incomplete pairs.")

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
    na.rm = na.rm,
    ...
  )

}


#' @noRd
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

  # 1.96 is the conventional LoA multiplier and is deliberately not tied
  # to conf.level - see the Details section.
  loaMult <- qnorm(0.975)

  loaLower <- bias - loaMult * sdDiff
  loaUpper <- bias + loaMult * sdDiff

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

      nObs = nObs,

      conf.level = conf.level

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
#' @param digits number of decimal places to display
#' @param ... further arguments passed to or from other methods
#'
#' @return
#' invisibly, \code{x}
#'
#' @method print BlandAltman
#' @export
print.BlandAltman <- function(
    x,
    digits = 3,
    ...
){

  # digits is used as a decimal-place count by sprintf("%.*f") below, so
  # the former default getOption("digits") - a *significant*-digit
  # setting, 7 by default - printed seven decimals for every figure.
  if(!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
     digits < 0 || digits %% 1 != 0)
    stop("'digits' must be a single non-negative whole number")

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
      "n = %d, conf.level = %s\n",
      x$nObs,
      format(x$conf.level)
    )
  )

  invisible(x)

}
