
#' Geometric Mean and Standard Deviation 
#' 
#' Calculates the geometric mean, its confidence interval and the geometric
#' standard deviation of a vector x. 
#' 
#' The geometric mean is defined as: \deqn{\sqrt[n]{x_{1}\cdot x_{2}\cdot x_{3}
#' \ldots \cdot x_{n}}}{(x_1 * x_2 * ... * x_n)^(1/n)}
#' 
#' The geometric mean and geometric standard deviation are restricted to
#' positive inputs (because otherwise the answer can have an imaginary
#' component). Hence if any argument is negative, the result will be \code{NA}.
#' If any argument is zero, then the geometric mean is zero.\cr For strict
#' positive values the geometric mean is computed as
#' \code{exp(meanCI(log(x)))}.
#' 
#' \bold{Considerations (Roenfeldt 2018)} \verb{ } "The calculation of the
#' geometric mean requires that all values are non-zero and positive. So what
#' should you do if you have data that do not meet this requirement? If you
#' have values that equal zero, you have a few options: \itemize{ \item Adjust
#' your scale so that you add 1 to every number in the data set, and then
#' subtract 1 from the resulting geometric mean. \item Ignore zeros or missing
#' data in your calculations. \item Convert zeros to a very small number (often
#' called "below the detection limit") that is less than the next smallest
#' number in the data set. }
#' 
#' If you have negative numbers, you will need to convert those numbers to a
#' positive value before calculating the geometric mean. You can then assign
#' the resulting geometric mean a negative value. If your data set contains
#' both positive and negative values, you will have to separate them and find
#' the geometric means for each group, and you can then find the weighted
#' average of their individual geometric means to find the total geometric mean
#' for the full data set. If none of these options appeals to you, you are not
#' alone! There is controversy among statisticians about what is the best
#' method for dealing with these values. You may want to calculate several
#' types of averages and decide what makes the most sense for you and the
#' results you are trying to report."
#' 
#' @aliases gmean gsd
#' 
#' @param x a positive numeric vector. An object which is not a vector is
#' coerced, if possible, by \code{as.vector()}.
#' 
#' @param conf.level confidence level of the interval. If set to \code{NA}
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of \code{"two.sided"} (default), \code{"left"} or
#'   \code{"right"}). See \code{\link{ConfidenceIntervals}}.
#'
#' @param method a vector of character strings representing the type of
#' intervals required. The value should be any subset of the values
#' \code{"classic"}, \code{"boot"}.  See \code{\link[boot]{boot.ci}}. 
#' 
#' @param na.rm logical, indicating whether \code{NA} values should be stripped
#' before the computation proceeds. Defaults to \code{FALSE}. 
#' @param ... further arguments are passed to the \code{\link[boot]{boot}}
#' function. Supported arguments are \code{type} (\code{"norm"},
#' \code{"basic"}, \code{"stud"}, \code{"perc"}, \code{"bca"}), \code{parallel}
#' and the number of bootstrap replicates \code{R}. If not defined those will
#' be set to their defaults, being \code{"basic"} for \code{type}, option
#' \code{"boot.parallel"} (and if that is not set, \code{"no"}) for
#' \code{parallel} and \code{999} for \code{R}.
#' 
#' @return for \code{gsd()}, a numeric scalar (\code{NA} if fewer than two
#' strictly positive values remain). For \code{gmean()}, a numeric
#' scalar if \code{conf.level = NA}; otherwise a named numeric vector with
#' elements:
#' \describe{
#'   \item{\code{est}}{point estimate of the geometric mean}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#' 
#' @references Snedecor, G. W., Cochran, W. G. Cochran (1989) Statistical
#' Methods, 8th ed. Ames, \emph{IA: Iowa State University Press }
#' 
#' Roenfeldt K. (2018) Better than Average: Calculating Geometric Means Using
#' SAS, Henry M. Jackson Foundation for the Advancement of Military Medicine,
#' \cr\url{https://www.lexjansen.com/wuss/2018/56_Final_Paper_PDF.pdf}
#' 
#' @examples
#' set.seed(1)
#' x <- runif(5)
#' gmean(x)
#' 
#' m <- matrix(runif(50), nrow = 10)
#' apply(m, 2, gmean)
#' 
#' sapply(as.data.frame(m), gmean)
#' 
#' 
#' # example in https://www.stata.com/manuals13/rameans.pdf
#' x <- c(5,4,-4,-5,0,0,NA,7)
#' 
#' # positives only
#' gmean(x[x>0], na.rm=TRUE, conf.level=0.95)
#' 
#' # shift by 5 so that everything is positive, then drop the zeros:
#' # naReplace() puts zeros IN (turning NA into 0), which makes the
#' # geometric mean collapse to 0 - the opposite of what is wanted here
#' z <- x + 5
#' gmean(z[!is.na(z) & z > 0], conf.level = 0.95)
#' 
#' @family location
#' @concept location
#' @concept nonlinear-mean
#' @rdname gmean
#' @export
gmean <- function (x, conf.level = NA, sides = c("two.sided","left","right"),
                   method = c("classic", "boot"),
                   na.rm = FALSE, ...) {
  
  # see also: http://www.stata.com/manuals13/rameans.pdf
  
  if(na.rm) x <- na.omit(x)
  
  if(any(is.na(x) | (is_neg <- x < 0))){
    
    if(any(na.omit(is_neg))) 
      warning("x contains negative values")
    
    if(is.na(conf.level))
      NA_real_
    else
      c(est = NA_real_, lci = NA_real_, uci = NA_real_)
    
  } else if(any(x==0)) {
    
    if(is.na(conf.level))
      0
    else
      c(est = 0, lci = NA, uci = NA)
    
  } else {
    
    if(is.na(conf.level))
      exp(mean(log(x)))
    
    else
      exp(meanCI(x=log(x), method = method,
                 conf.level = conf.level, sides = sides, ...))
  }
  
}




#' @rdname gmean
#' @export
gsd <- function (x, na.rm = FALSE) {

  # Non-positive values become NA here, i.e. AFTER na.omit() has run, so
  # sd() saw them and returned NA even when na.rm = TRUE was requested.
  is.na(x) <- x <= 0

  if(na.rm) x <- as.numeric(na.omit(x))

  if(length(x) < 2L)
    return(NA_real_)

  exp(sd(log(x), na.rm = na.rm))
}
