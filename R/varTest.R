
#' ChiSquare Test for One Variance and F Test to Compare Two Variances
#' 
#' Performs either a one sample chi-squared test to compare the variance of a
#' vector with a given value or an F test to compare the variances of two
#' samples from normal populations.
#' 
#' The formula interface is only applicable for the 2-sample tests.
#' 
#' The null hypothesis is that the ratio of the variances of the populations
#' from which \code{x} and \code{y} were drawn, or in the data to which the
#' linear models \code{x} and \code{y} were fitted, is equal to \code{ratio}.
#' 
#' @name varTest
#' @aliases varTest varTest.default varTest.formula
#' @param x,y numeric vectors of data values.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of \code{"two.sided"} (default), \code{"greater"} or
#' \code{"less"}.  You can specify just the initial letter.
#' @param ratio the hypothesized ratio of the population variances of \code{x}
#' and \code{y}.
#' @param sigma.squared a number indicating the true value of the variance, if
#' one sample test is requested.
#' @param conf.level confidence level for the returned confidence interval.
#' @param formula a formula of the form \code{lhs ~ rhs} where \code{lhs} is a
#' numeric variable giving the data values and \code{rhs} a factor with two
#' levels giving the corresponding groups.
#' @param data an optional matrix or data frame (or similar: see
#' \code{\link{model.frame}}) containing the variables in the formula
#' \code{formula}.  By default the variables are taken from
#' \code{environment(formula)}.
#' @param subset an optional vector specifying a subset of observations to be
#' used.
#' @param na.action a function which indicates what should happen when the data
#' contain \code{NA}s.  Defaults to \code{getOption("na.action")}.
#' @param \dots further arguments to be passed to or from methods.
#' @return A list with class \code{"htest"} containing the following
#' components: \item{statistic}{the value of the F test statistic.}
#' \item{parameter}{the degrees of the freedom of the F distribution of the
#' test statistic.} \item{p.value}{the p-value of the test.} \item{conf.int}{a
#' confidence interval for the ratio of the population variances.}
#' \item{estimate}{the ratio of the sample variances of \code{x} and \code{y}.}
#' \item{null.value}{the ratio of population variances under the null.}
#' \item{alternative}{a character string describing the alternative
#' hypothesis.} \item{method}{the character string \code{"F test to compare two
#' variances"}.} \item{data.name}{a character string giving the names of the
#' data.}
#' @author Andri Signorell <andri@@signorell.net> (One sample test)\cr Two
#' Sample test and help text from R-Core.
#' @seealso \code{\link{var.test}}, \code{\link{bartlett.test}} for testing
#' homogeneity of variances in more than two samples from normal distributions;
#' \code{\link{ansari.test}} and \code{\link{mood.test}} for two rank based
#' (nonparametric) two-sample tests for difference in scale.
#' @keywords htest
#' @examples
#' 
#' x <- rnorm(50, mean = 0, sd = 2)
#' 
#' # One sample test
#' varTest(x, sigma.squared = 2.5)
#' 
#' # two samples
#' y <- rnorm(30, mean = 1, sd = 1)
#' varTest(x, y)                  # Do x and y have the same variance?
#' varTest(lm(x ~ 1), lm(y ~ 1))  # The same.
#' 
#' 
#' 

#' @rdname varTest
#' @export
varTest <- function(x, ...) UseMethod("varTest")


#' @rdname varTest
#' @export
varTest.default <- function (x, y = NULL, alternative = c("two.sided", "less", "greater"), ratio = 1,
                             sigma.squared = 1, conf.level = 0.95, ...) {
  
  
  .twoSided_pval <- function(x, DF){
    
    # https://stats.stackexchange.com/questions/195469/calculating-p-values-for-two-tail-test-for-population-variance
    
    # What you are dealing with in this question is a two-sided 
    # variance test, which is a specific case of a two-sided test 
    # with an asymmetric null distribution. The p-value is the total 
    # area under the null density for all values in the lower and 
    # upper tails of that density that are at least as "extreme" 
    # (i.e., at least as conducive to the alternative hypothesis) 
    # as the observed test statistic. Because this test has an 
    # asymmetric null distribution, we need to specify exactly 
    # what we mean by "extreme".
    # 
    # Lowest-density p-value calculation: The most sensible thing 
    # method of two-sided hypothesis testing is to interpret 
    # "more extreme" as meaning a lower value of the null density. 
    # This is the interpretation used in a standard likelihood-ratio 
    # (LR) test. Under this method , the p-value is the probability of 
    # falling in the "lowest density region", where the density 
    # cut-off is the density at the observed test statistic. With 
    # an asymmetric null distribution, this leads you to a p-value 
    # calculated with unequal tails.
    
    # example:
    #   TwoSided_pval(15.35667, DF=17)
    #   TwoSided_pval(14.6489, DF=17)
    
    
    InvDChisq <- function(x2, DF){
      
      fun <- function(x) dchisq(x, df=DF) - dchisq(x2, df=DF)
      # the mode of chisq distribution
      mod_x2 <- DF-2
      
      if(x2 < mod_x2){
        # don't know how to set the right boundary in a sensible way
        # the treshold 1e12 is selected randomly
        # benchmark show no difference in performance between 1e6 and 1e12
        unirootAll(fun, interval = c(mod_x2, 1e12))
      } else {
        unirootAll(fun, interval = c(0, mod_x2))
      }
    }
    
    
    pchisq(x, df = DF, lower.tail = x < DF-2) + 
      pchisq(InvDChisq(x, DF), df = DF, lower.tail=!x < DF-2)
    
  }
  
  
  if(is.null(y)){
    # perform a one sample variance test
    
    alternative <- match.arg(alternative)
    if (!missing(sigma.squared) && (length(sigma.squared) != 1 || is.na(sigma.squared)))
      stop("'sigma.squared' must be a single number")
    
    if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) ||
                                 conf.level < 0 || conf.level > 1))
      stop("'conf.level' must be a single number between 0 and 1")
    
    dname <- deparse(substitute(x))
    x <- na.omit(x)
    
    nx <- length(x)
    if (nx < 2)
      stop("not enough 'x' observations")
    df <- nx - 1
    vx <- var(x)
    
    xstat <- vx * df / sigma.squared
    
    method <- "One Sample Chi-Square test on variance"
    estimate <- vx
    
    if (alternative == "less") {
      pval <- pchisq(xstat, df)
      cint <- c(0, df * vx/qchisq((1 - conf.level), df))
      
    } else if (alternative == "greater") {
      pval <- pchisq(xstat, df, lower.tail = FALSE)
      cint <- c(df * vx/qchisq((1 - conf.level), df, lower.tail = FALSE), Inf)
      
    } else {
      # this is a "quick-and-nasty" approximation, let's use a better one..
      # pval <- 2 * min(pchisq(xstat, df), 
      #                 pchisq(xstat, df, lower.tail = FALSE))
      pval <- .twoSided_pval(xstat, df)
      
      alpha <- 1 - conf.level
      cint <- df * vx / c(qchisq((1 - conf.level)/2, df, lower.tail = FALSE),
                          qchisq((1 - conf.level)/2, df))
    }
    
    names(xstat) <- "X-squared"
    names(df) <- "df"
    names(sigma.squared) <- "variance"
    names(estimate) <- "variance of x"
    attr(cint, "conf.level") <- conf.level
    rval <- list(statistic = xstat, parameter = df, p.value = pval,
                 conf.int = cint, estimate = estimate, null.value = sigma.squared,
                 alternative = alternative, method = method, data.name = dname)
    class(rval) <- "htest"
    
    return(rval)
    
  } else {
    # perform a normal F-test
    var.test(x=x, y=y, ratio=ratio, alternative=alternative, conf.level=conf.level)
  }
  
}


#' @rdname varTest
#' @export
varTest.formula <- function (formula, data, subset, na.action, ...) {
  
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L)
    stop("grouping factor must have exactly 2 levels")
  DATA <- setNamesX(split(mf[[response]], g), c("x", "y"))
  y <- do.call("varTest", c(DATA, list(...)))
  y$data.name <- DNAME
  y
  
}
