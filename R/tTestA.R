
#' Student's t-Test Based on Sample Statistics
#' 
#' Performs one and two sample t-tests based on user supplied summary
#' information instead of data as in \code{t.test()}.
#' 
#' \code{alternative = "greater"} is the alternative that \code{x} has a larger
#' mean than \code{y}.
#' 
#' The option \code{paired} is not supported here, as the variance of the
#' differences can't be calculated on the base of the variances of the two
#' samples. However, for calculating the paired test we can simply supply the
#' mean and standard deviation of the differences and use the one-sample test
#' with \code{mu = 0}.
#' 
#' If \code{var.equal} is \code{TRUE} then the pooled estimate of the variance
#' is used.  By default, if \code{var.equal} is \code{FALSE} then the variance
#' is estimated separately for both groups and the Welch modification to the
#' degrees of freedom is used.
#' 
#' If the input data are effectively constant (compared to the larger of the
#' two means) an error is generated.
#' 
#' @param mx a single number representing the sample mean of x.
#' @param my an optional single number representing the sample mean of y.
#' @param sx a single number representing the sample standard deviation of x.
#' @param sy an optional single number representing the sample standard
#' deviation of y.
#' @param nx a single number representing the sample size of x.
#' @param ny an optional single number representing the sample size of y.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of \code{"two.sided"} (default), \code{"greater"} or
#' \code{"less"}.  You can specify just the initial letter.
#' @param mu a number indicating the true value of the mean (or difference in
#' means if you are performing a two sample test).
#' @param paired paired = \code{TRUE} is not supported here and only present
#' for consistency of arguments. Use the one-sample-test for the differences
#' instead.
#' @param var.equal a logical variable indicating whether to treat the two
#' variances as being equal. If \code{TRUE} then the pooled variance is used to
#' estimate the variance otherwise the Welch (or Satterthwaite) approximation
#' to the degrees of freedom is used.
#' @param conf.level confidence level of the interval.
#' @param \dots further arguments to be passed to or from methods.
#' @return A list with class \code{"htest"} containing the following
#' components: \item{statistic}{the value of the t-statistic.}
#' \item{parameter}{the degrees of freedom for the t-statistic.}
#' \item{p.value}{the p-value for the test.} \item{conf.int}{a confidence
#' interval for the mean appropriate to the specified alternative hypothesis.}
#' \item{estimate}{the estimated mean or difference in means depending on
#' whether it was a one-sample test or a two-sample test.}
#' \item{null.value}{the specified hypothesized value of the mean or mean
#' difference depending on whether it was a one-sample test or a two-sample
#' test.} \item{alternative}{a character string describing the alternative
#' hypothesis.} \item{method}{a character string indicating what type of t-test
#' was performed.} \item{data.name}{a character string giving the name(s) of
#' the data.}
#' @seealso \code{\link{t.test}}
#' @keywords htest
#' @examples
#' 
#' ## Classical example: Student's sleep data
#' mx <- 0.75
#' my <- 2.33
#' sx <- 1.789010
#' sy <- 2.002249
#' nx <- ny <- 10
#' tTestA(mx=mx, my=my, sx=sx, sy=sy, nx=nx, ny=ny)
#' 
#' # compare to
#' with(sleep, t.test(extra[group == 1], extra[group == 2]))
#' 
#' # use the one sample test for the differences instead of paired=TRUE option
#' x <- with(sleep, extra[group == 1])
#' y <- with(sleep, extra[group == 2])
#' 
#' tTestA(mx=mean(x-y), sx=sd(x-y), nx=length(x-y))
#' 
#' # compared to 
#' t.test(x, y, paired = TRUE)
#' 
#' 
 

#' @export
tTestA <- function (mx, sx, nx, my=NULL, sy = NULL, ny=NULL,
                    alternative = c("two.sided", "less", "greater"),
                    mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95,
                    ...) {
  
  if (paired)
    stop("paired option is not supported, use one-sample-test for the differences instead")
  
  alternative <- match.arg(alternative)
  if (!missing(mu) && (length(mu) != 1 || is.na(mu)))
    stop("'mu' must be a single number")
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) ||
                               conf.level < 0 || conf.level > 1))
    stop("'conf.level' must be a single number between 0 and 1")
  
  if (!is.null(my)) {
    dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    
  } else {
    dname <- deparse(substitute(x))
    if (paired)
      stop("'y' is missing for paired test")
  }
  
  vx <- sx^2
  
  if (is.null(my)) {
    if (nx < 2)
      stop("not enough 'x' observations")
    df <- nx - 1
    stderr <- sqrt(vx/nx)
    if (stderr < 10 * .Machine$double.eps * abs(mx))
      stop("data are essentially constant")
    tstat <- (mx - mu)/stderr
    method <- if (paired)
      "Paired t-test"
    else "One Sample t-test"
    estimate <- setNamesX(mx, if (paired)
      "mean of the differences"
      else "mean of x")
    
  } else {
    # ny <- length(y)
    if (nx < 1 || (!var.equal && nx < 2))
      stop("not enough 'x' observations")
    if (ny < 1 || (!var.equal && ny < 2))
      stop("not enough 'y' observations")
    if (var.equal && nx + ny < 3)
      stop("not enough observations")
    # my <- mean(y)
    # vy <- var(y)
    vy <- sy^2
    method <- paste(if (!var.equal)
      "Welch", "Two Sample t-test")
    estimate <- c(mx, my)
    names(estimate) <- c("mean of x", "mean of y")
    if (var.equal) {
      df <- nx + ny - 2
      v <- 0
      if (nx > 1)
        v <- v + (nx - 1) * vx
      if (ny > 1)
        v <- v + (ny - 1) * vy
      v <- v/df
      stderr <- sqrt(v * (1/nx + 1/ny))
    }
    else {
      stderrx <- sqrt(vx/nx)
      stderry <- sqrt(vy/ny)
      stderr <- sqrt(stderrx^2 + stderry^2)
      df <- stderr^4/(stderrx^4/(nx - 1) + stderry^4/(ny - 1))
    }
    if (stderr < 10 * .Machine$double.eps * max(abs(mx),
                                                abs(my)))
      stop("data are essentially constant")
    tstat <- (mx - my - mu)/stderr
  }
  if (alternative == "less") {
    pval <- pt(tstat, df)
    cint <- c(-Inf, tstat + qt(conf.level, df))
  }
  else if (alternative == "greater") {
    pval <- pt(tstat, df, lower.tail = FALSE)
    cint <- c(tstat - qt(conf.level, df), Inf)
  }
  else {
    pval <- 2 * pt(-abs(tstat), df)
    alpha <- 1 - conf.level
    cint <- qt(1 - alpha/2, df)
    cint <- tstat + c(-cint, cint)
  }
  cint <- mu + cint * stderr
  names(tstat) <- "t"
  names(df) <- "df"
  names(mu) <- if (paired || !is.null(my))
    "difference in means"
  else "mean"
  attr(cint, "conf.level") <- conf.level
  rval <- list(statistic = tstat, parameter = df, p.value = pval,
               conf.int = cint, estimate = estimate, null.value = mu, stderr = stderr, 
               alternative = alternative, method = method, data.name = dname)
  class(rval) <- "htest"
  return(rval)
}

