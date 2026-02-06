
#' Pearson chi-square test for normality
#' 
#' Performs the Pearson chi-square test for the composite hypothesis of
#' normality, see e.g. Thode (2002, Sec. 5.2).
#' 
#' The Pearson test statistic is \eqn{P=\sum (C_{i} - E_{i})^{2}/E_{i}}, where
#' \eqn{C_{i}} is the number of counted and \eqn{E_{i}} is the number of
#' expected observations (under the hypothesis) in class \eqn{i}. The classes
#' are build is such a way that they are equiprobable under the hypothesis of
#' normality. The p-value is computed from a chi-square distribution with
#' \code{n.classes}-3 degrees of freedom if \code{adjust} is \code{TRUE} and
#' from a chi-square distribution with \code{n.classes}-1 degrees of freedom
#' otherwise. In both cases this is not (!) the correct p-value, lying
#' somewhere between the two, see also Moore (1986).
#' 
#' @param x a numeric vector of data values. Missing values are allowed.
#' @param n.classes The number of classes. The default is due to Moore (1986).
#' @param adjust logical; if \code{TRUE} (default), the p-value is computed
#' from a chi-square distribution with \code{n.classes}-3 degrees of freedom,
#' otherwise from a chi-square distribution with \code{n.classes}-1 degrees of
#' freedom.
#' @return A list with class \dQuote{htest} containing the following
#' components: \item{statistic}{the value of the Pearson chi-square statistic.}
#' \item{p.value }{the p-value for the test.} \item{method}{the character
#' string \dQuote{Pearson chi-square normality test}.} \item{data.name}{a
#' character string giving the name(s) of the data.} \item{n.classes}{the
#' number of classes used for the test.} \item{df}{the degress of freedom of
#' the chi-square distribution used to compute the p-value.}
#' @note The Pearson chi-square test is usually not recommended for testing the
#' composite hypothesis of normality due to its inferior power properties
#' compared to other tests. It is common practice to compute the p-value from
#' the chi-square distribution with \code{n.classes} - 3 degrees of freedom, in
#' order to adjust for the additional estimation of two parameters. (For the
#' simple hypothesis of normality (mean and variance known) the test statistic
#' is asymptotically chi-square distributed with \code{n.classes} - 1 degrees
#' of freedom.) This is, however, not correct as long as the parameters are
#' estimated by \code{mean(x)} and \code{var(x)} (or \code{sd(x)}), as it is
#' usually done, see Moore (1986) for details. Since the true p-value is
#' somewhere between the two, it is suggested to run \code{pearsonTest} twice,
#' with \code{adjust = TRUE} (default) and with \code{adjust = FALSE}. It is
#' also suggested to slightly change the default number of classes, in order to
#' see the effect on the p-value. Eventually, it is suggested not to rely upon
#' the result of the test.
#' 
#' The function call \code{pearsonTest(x)} essentially produces the same
#' result as the S-PLUS function call \code{chisq.gof((x-mean(x))/sqrt(var(x)),
#' n.param.est=2)}.
#' 
#' @author Juergen Gross
#' @seealso \code{\link{shapiro.test}} for performing the Shapiro-Wilk test for
#' normality. \code{\link{andersonDarlingTest}}, \code{\link{cramerVonMisesTest}},
#' \code{\link{lillieTest}}, \code{\link{shapiroFranciaTest}} for performing further
#' tests for normality. \code{\link{qqnorm}} for producing a normal
#' quantile-quantile plot.
#' @references Moore, D.S. (1986): Tests of the chi-squared type. In:
#' D'Agostino, R.B. and Stephens, M.A., eds.: Goodness-of-Fit Techniques.
#' Marcel Dekker, New York.
#' 
#' Thode Jr., H.C. (2002): Testing for Normality. Marcel Dekker, New York.
#' @keywords htest
#' @examples
#' 
#' pearsonTest(rnorm(100, mean = 5, sd = 3))
#' pearsonTest(runif(100, min = 2, max = 4))
#' 

#' @export
pearsonTest <- function (x, n.classes = ceiling(2 * (n^(2/5))), 
                         adjust = TRUE) {
  
    DNAME <- deparse(substitute(x))
    x <- x[complete.cases(x)]
    n <- length(x)
    if (adjust) {
        dfd <- 2
    }
    else {
        dfd <- 0
    }
    num <- floor(1 + n.classes * pnorm(x, mean(x), sd(x)))
    count <- tabulate(num, n.classes)
    prob <- rep(1/n.classes, n.classes)
    xpec <- n * prob
    h <- ((count - xpec)^2)/xpec
    P <- sum(h)
    pvalue <- pchisq(P, n.classes - dfd - 1, lower.tail = FALSE)
    RVAL <- list(statistic = c(P = P), p.value = pvalue, 
                 method = "Pearson chi-square normality test", 
        data.name = DNAME, n.classes = n.classes, 
        df = n.classes - 1 - dfd)
    
    class(RVAL) <- "htest"
    return(RVAL)
    
}
