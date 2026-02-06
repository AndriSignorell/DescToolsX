
#' Sign Test
#' 
#' Performs one- and two-sample sign tests on vectors of data.
#' 
#' The formula interface is only applicable for the 2-sample test.
#' 
#' \code{signTest} computes a \dQuote{Dependent-samples Sign-Test} if both
#' \code{x} and \code{y} are provided.  If only \code{x} is provided, the
#' \dQuote{One-sample Sign-Test} will be computed.
#' 
#' For the one-sample sign-test, the null hypothesis is that the median of the
#' population from which \code{x} is drawn is \code{mu}. For the two-sample
#' dependent case, the null hypothesis is that the median for the differences
#' of the populations from which \code{x} and \code{y} are drawn is \code{mu}.
#' The alternative hypothesis indicates the direction of divergence of the
#' population median for \code{x} from \code{mu} (i.e., \code{"greater"},
#' \code{"less"}, \code{"two.sided"}.)
#' 
#' The confidence levels are exact.
#' 
#' @aliases signTest signTest.default signTest.formula
#' @param x numeric vector of data values. Non-finite (e.g. infinite or
#' missing) values will be omitted.
#' @param y an optional numeric vector of data values: as with x non-finite
#' values will be omitted.
#' @param mu a number specifying an optional parameter used to form the null
#' hypothesis. See Details.
#' @param alternative is a character string, one of \code{"greater"},
#' \code{"less"}, or \code{"two.sided"}, or the initial letter of each,
#' indicating the specification of the alternative hypothesis. For one-sample
#' tests, \code{alternative} refers to the true median of the parent population
#' in relation to the hypothesized value of the median.
#' @param conf.level confidence level for the returned confidence interval,
#' restricted to lie between zero and one.
#' 
#' @return A list of class \code{htest}, containing the following components:
#' \item{statistic}{ the S-statistic (the number of positive differences
#' between the data and the hypothesized median), with names attribute
#' \dQuote{S}.} \item{parameter}{ the total number of valid differences.}
#' \item{p.value}{ the p-value for the test.} \item{null.value}{is the value of
#' the median specified by the null hypothesis. This equals the input argument
#' \code{mu}. } \item{alternative}{a character string describing the
#' alternative hypothesis.} \item{method}{ the type of test applied.}
#' \item{data.name}{a character string giving the names of the data.}
#' \item{conf.int}{ a confidence interval for the median.} \item{estimate}{ the
#' sample median.}
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{t.test}}, \code{\link{wilcox.test}},
#' \code{\link{zTest}}, \code{\link{binom.test}}, \code{\link[BSDA]{SIGN.test}}
#' in the package \pkg{BSDA} (reporting approximative confidence intervals).
#' @references Gibbons, J.D. and Chakraborti, S. (1992): \emph{Nonparametric
#' Statistical Inference}. Marcel Dekker Inc., New York.
#' 
#' Kitchens, L. J. (2003): \emph{Basic Statistics and Data Analysis}. Duxbury.
#' 
#' Conover, W. J. (1980): \emph{Practical Nonparametric Statistics, 2nd ed}.
#' Wiley, New York.
#' @keywords htest
#' @examples
#' 
#' x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
#' y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
#' 
#' signTest(x, y)
#' wilcox.test(x, y, paired = TRUE)
#' 
#' 
#' d.light <- data.frame( 
#'   black = c(25.85,28.84,32.05,25.74,20.89,41.05,25.01,24.96,27.47),
#'   white = c(18.23,20.84,22.96,19.68,19.5,24.98,16.61,16.07,24.59),
#'   d     = c(7.62,8,9.09,6.06,1.39,16.07,8.4,8.89,2.88)
#' )
#' 
#' d <- d.light$d
#' 
#' signTest(x=d, mu = 4)
#' wilcox.test(x=d, mu = 4, conf.int = TRUE)
#' 
#' signTest(x=d, mu = 4, alternative="less")
#' wilcox.test(x=d, mu = 4, conf.int = TRUE, alternative="less")
#' 
#' signTest(x=d, mu = 4, alternative="greater")
#' wilcox.test(x=d, mu = 4, conf.int = TRUE, alternative="greater")
#' 
#' with(d.light, signTest(black, white))
#' # same as:
#' with(d.light, signTest(black - white))
#' 


# Note: Formula only where it is semantically relevant.
# it does not make sense to implement formula interface for 
# onesample/paired design, as the order might not be given.
# wilcox.test produces Error: cannot use 'paired' in formula method!


#' @rdname signTest
#' @export
signTest <- function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
                     mu = 0, conf.level = 0.95) {
  
  alternative <- match.arg(alternative)
  
  if (!missing(mu) && ((length(mu) > 1L) || !is.finite(mu)))
    stop("'mu' must be a single number")
  
  if (!((length(conf.level) == 1L) && is.finite(conf.level) &&
        (conf.level > 0) && (conf.level < 1)))
    stop("'conf.level' must be a single number between 0 and 1")
  
  if (!is.numeric(x))
    stop("'x' must be numeric")
  
  if (!is.null(y)) {
    if (!is.numeric(y))
      stop("'y' must be numeric")
    
    if (length(x) != length(y))
      stop("'x' and 'y' must have the same length")
    
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    OK <- complete.cases(x, y)
    x <- x[OK]
    y <- y[OK]
    METHOD <- "Dependent-samples Sign-Test"
    x <- (x - y)
    
  } else {
    DNAME <- deparse(substitute(x))
    x <- x[is.finite(x)]
    METHOD <- "One-sample Sign-Test"
  }
  
  d <- (x - mu)
  
  # Naive version:
  n.valid <- sum(d > 0) + sum(d < 0)
  if(n.valid > 0) {
    RVAL <- binom.test(x=sum(d > 0), n=n.valid, p=0.5, alternative = alternative, conf.level = conf.level )
  } else {
    RVAL <- binom.test(x=1, n=1)
  }
  
  RVAL$method <- METHOD
  RVAL$data.name <- DNAME
  names(mu) <- if (!is.null(y)) "median difference" else "median"
  
  names(RVAL$statistic) <- "S"
  RVAL$estimate <- median(d + mu, na.rm=TRUE)
  names(RVAL$parameter) <- "number of differences"
  mci <- medianCI(d + mu, conf.level=conf.level, 
                  sides=if(alternative=="less") "right" else if(alternative=="greater") "left" else "two.sided", na.rm=TRUE)
  RVAL$conf.int <- mci[-1]
  attr(RVAL$conf.int, "conf.level") = round(attr(mci,"conf.level"), 3)
  
  names(RVAL$estimate) <- "median of the differences"
  RVAL$null.value <- mu
  class(RVAL) <- "htest"
  return(RVAL)
  
}

