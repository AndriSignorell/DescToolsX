

#' Shapiro-Francia test for normality
#' 
#' Performs the Shapiro-Francia test for the composite hypothesis of normality,
#' see e.g. Thode (2002, Sec. 2.3.2).
#' 
#' The test statistic of the Shapiro-Francia test is simply the squared
#' correlation between the ordered sample values and the (approximated)
#' expected ordered quantiles from the standard normal distribution. The
#' p-value is computed from the formula given by Royston (1993).
#' 
#' @param x a numeric vector of data values, the number of which must be
#' between 5 and 5000. Missing values are allowed.
#' @return A list with class \dQuote{htest} containing the following
#' components: \item{statistic}{the value of the Shapiro-Francia statistic.}
#' \item{p.value }{the p-value for the test.} \item{method}{the character
#' string \dQuote{Shapiro-Francia normality test}.} \item{data.name}{a
#' character string giving the name(s) of the data.}
#' @note The Shapiro-Francia test is known to perform well, see also the
#' comments by Royston (1993). The expected ordered quantiles from the standard
#' normal distribution are approximated by \code{qnorm(ppoints(x, a = 3/8))},
#' being slightly different from the approximation \code{qnorm(ppoints(x, a =
#' 1/2))} used for the normal quantile-quantile plot by \code{\link{qqnorm}}
#' for sample sizes greater than 10.
#' @author Juergen Gross
#' @seealso \code{\link{shapiro.test}} for performing the Shapiro-Wilk test for
#' normality.  
#' \code{\link{andersonDarlingTest}}, \code{\link{cramerVonMisesTest}},
#' \code{\link{lillieTest}}, 
#' \code{\link{pearsonTest}} for performing further
#' tests for normality. \code{\link{qqnorm}} for producing a normal
#' quantile-quantile plot.
#' @references Royston, P. (1993): A pocket-calculator algorithm for the
#' Shapiro-Francia test for non-normality: an application to medicine.
#' Statistics in Medicine, 12, 181--184.
#' 
#' Thode Jr., H.C. (2002): Testing for Normality. Marcel Dekker, New York.
#' @keywords htest
#' @examples
#' 
#' shapiroFranciaTest(rnorm(100, mean = 5, sd = 3))
#' shapiroFranciaTest(runif(100, min = 2, max = 4))
#'
#'


#'@export 
shapiroFranciaTest <- function (x) {
  
    DNAME <- deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    n <- length(x)
    if ((n < 5 || n > 5000)) 
        stop("sample size must be between 5 and 5000")
    y <- qnorm(ppoints(n, a = 3/8))
    W <- cor(x, y)^2
    u <- log(n)
    v <- log(u)
    mu <- -1.2725 + 1.0521 * (v - u)
    sig <- 1.0308 - 0.26758 * (v + 2/u)
    z <- (log(1 - W) - mu)/sig
    pval <- pnorm(z, lower.tail = FALSE)
    RVAL <- list(statistic = c(W = W), p.value = pval, 
                 method = "Shapiro-Francia normality test", 
        data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
