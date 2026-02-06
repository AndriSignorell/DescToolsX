
#' Z Test for Known Population Standard Deviation
#' 
#' Compute the test of hypothesis and compute confidence interval on the mean
#' of a population when the standard deviation of the population is known.
#' 
#' Most introductory statistical texts introduce inference by using the z-test
#' and z-based confidence intervals based on knowing the population standard
#' deviation. However statistical packages often do not include functions to do
#' z-tests since the t-test is usually more appropriate for real world
#' situations. This function is meant to be used during that short period of
#' learning when the student is learning about inference using z-procedures,
#' but has not learned the t-based procedures yet.  Once the student has
#' learned about the t-distribution the \code{t.test()} function should be used
#' instead of this one (but the syntax is very similar, so this function should
#' be an appropriate introductory step to learning \code{t.test()}).
#' 
#' The formula interface is only applicable for the 2-sample tests.
#' 
#' @name zTest
#' @aliases zTest zTest.default zTest.formula
#' @param x numeric vector of data values. Non-finite (e.g. infinite or
#' missing) values will be omitted.
#' @param y an optional numeric vector of data values: as with x non-finite
#' values will be omitted.
#' @param mu a number specifying the hypothesized mean of the population.
#' @param sd_pop a number specifying the known standard deviation of the
#' population.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of \code{"two.sided"} (default), \code{"greater"} or
#' \code{"less"}.  You can specify just the initial letter. \cr For one-sample
#' tests, \code{alternative} refers to the true mean of the parent population
#' in relation to the hypothesized value of the mean.
#' @param paired a logical indicating whether you want a paired z-test.
#' @param conf.level confidence level for the interval computation.
#' @param formula a formula of the form \code{lhs ~ rhs} where \code{lhs} gives
#' the data values and \code{rhs} a factor with two levels giving the
#' corresponding groups.
#' @param data an optional matrix or data frame (or similar: see
#' \code{\link{model.frame}}) containing the variables in the formula
#' \code{formula}.  By default the variables are taken from
#' \code{environment(formula)}.
#' @param subset an optional vector specifying a subset of observations to be
#' used.
#' @param na.action a function which indicates what should happen when the data
#' contain \code{NA}s. Defaults to \code{getOption("na.action")}.
#' @param \dots further arguments to be passed to or from methods.
#' @return A list with class "\code{htest}" containing the following
#' components: \item{statistic}{ the value of the z-statistic.} \item{p.value}{
#' the p-value for the test} \item{conf.int}{a confidence interval for the mean
#' appropriate to the specified alternative hypothesis.} \item{estimate}{the
#' estimated mean or difference in means depending on whether it was a
#' one-sample test or a two-sample test.} \item{null.value}{the specified
#' hypothesized value of the mean or mean difference depending on whether it
#' was a one-sample test or a two-sample test.} \item{alternative}{a character
#' string describing the alternative hypothesis.} \item{method}{ a character
#' string indicating what type of test was performed.} \item{data.name}{a
#' character string giving the name(s) of the data.}
#' @author Andri Signorell <andri@@signorell.net>, based on R-Core code of
#' \code{\link{t.test}},\cr documentation partly from Greg Snow
#' <greg.snow@@imail.org>
#' @seealso \code{\link{t.test}}, \code{\link{print.htest}}
#' @references Stahel, W. (2002) \emph{Statistische Datenanalyse, 4th ed},
#' vieweg
#' @keywords htest
#' @examples
#' 
#' x <- rnorm(25, 100, 5)
#' zTest(x, mu=99, sd_pop=5)
#' 
#' # the classic interface
#' with(sleep, zTest(extra[group == 1], extra[group == 2], sd_pop=2))
#' 
#' # the formula interface
#' zTest(extra ~ group, data = sleep, sd_pop=2)
#' 
#' 
#' # Stahel (2002), pp. 186, 196
#' 
#' d.tyres <- data.frame(A=c(44.5,55,52.5,50.2,45.3,46.1,52.1,50.5,50.6,49.2),
#'                       B=c(44.9,54.8,55.6,55.2,55.6,47.7,53,49.1,52.3,50.7))
#' with(d.tyres, zTest(A, B, sd_pop=3, paired=TRUE))
#' 
#' 
#' d.oxen <- data.frame(ext=c(2.7,2.7,1.1,3.0,1.9,3.0,3.8,3.8,0.3,1.9,1.9),
#'                      int=c(6.5,5.4,8.1,3.5,0.5,3.8,6.8,4.9,9.5,6.2,4.1))
#' with(d.oxen, zTest(int, ext, sd_pop=1.8, paired=FALSE))
#' 

#' @rdname zTest
#' @export
zTest <- function (x, ...)
  UseMethod("zTest")


#' @rdname zTest
#' @export
zTest.formula <- function (formula, data, subset, na.action, ...)  {

    if (missing(formula) || length(formula) != 3L)
      stop("'formula' missing or incorrect")
    
    ## determine design
    oneSampleOrPaired <- FALSE
    if (length(attr(terms(formula[-2L]), "term.labels")) != 1L) {
      if (formula[[3L]] == 1L) {
        oneSampleOrPaired <- TRUE
      } else {
        stop("'formula' missing or incorrect")
      }
    }
    
    ## capture call for model.frame
    m <- match.call(expand.dots = FALSE)
    m$... <- NULL
    
    ## IMPORTANT: subset handling
    m$subset <- if (!missing(subset)) {
      substitute(subset)
    } else {
      m$subset <- NULL
    }
    
    ## IMPORTANT: na.action handling
    if (!missing(na.action)) {
      m$na.action <- substitute(na.action)
    } else {
      m$na.action <- NULL
    }
    
    if (is.matrix(eval(m$data, parent.frame())))
      m$data <- as.data.frame(data)
    
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval(m, parent.frame())
    
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    
    response <- attr(attr(mf, "terms"), "response")
    
    ## --- two-sample ---
    if (!oneSampleOrPaired) {
      
      g <- factor(mf[[-response]])
      if (nlevels(g) != 2L)
        stop("grouping factor must have exactly 2 levels")
      
      DATA <- split(mf[[response]], g)
      
      y <- t.test(
        x = DATA[[1L]],
        y = DATA[[2L]],
        ...
      )
      
    } else {
      
      resp <- mf[[response]]
      
      ## paired
      if (inherits(resp, "Pair")) {
        y <- t.test(
          x = resp[, 1L],
          y = resp[, 2L],
          paired = TRUE,
          ...
        )
      } else {
        ## one-sample
        y <- t.test(
          x = resp,
          ...
        )
      }
    }
    
    y$data.name <- DNAME
    y

}


#' @rdname zTest
#' @export
zTest.default <- function (x, y = NULL, alternative = c("two.sided", "less", "greater"),
                           paired = FALSE, mu = 0, sd_pop, conf.level = 0.95,  ...)  {
  
  alternative <- match.arg(alternative)
  if (!missing(mu) && (length(mu) != 1 || is.na(mu)))
    stop("'mu' must be a single number")
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) ||
                               conf.level < 0 || conf.level > 1))
    stop("'conf.level' must be a single number between 0 and 1")
  
  if (!is.null(y)) {
    dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
    
    if (paired)
      xok <- yok <- complete.cases(x, y)
    else {
      yok <- !is.na(y)
      xok <- !is.na(x)
    }
    
    y <- y[yok]
    
  } else {
    dname <- deparse(substitute(x))
    if (paired)
      stop("'y' is missing for paired test")
    xok <- !is.na(x)
    yok <- NULL
  }
  x <- x[xok]
  
  if (paired) {
    x <- x - y
    y <- NULL
  }
  
  nx <- length(x)
  mx <- mean(x)
  # vx <- sd_pop^2
  
  if (is.null(y)) {
    if (nx < 2)
      stop("not enough 'x' observations")
    stderr <- sqrt(sd_pop^2/nx)
    if (stderr < 10 * .Machine$double.eps * abs(mx))
      stop("data are essentially constant")
    zstat <- (mx - mu)/stderr
    
    method <- method <- if (paired)
      "Paired z-test" else "One Sample z-test"
    estimate <- setNamesX(mx, if (paired)
      "mean of the differences"
      else "mean of x")
  }
  else {
    ny <- length(y)
    if (nx < 1)
      stop("not enough 'x' observations")
    if (ny < 1)
      stop("not enough 'y' observations")
    if (nx + ny < 3)
      stop("not enough observations")
    my <- mean(y)
    
    method <- paste("Two Sample z-test")
    estimate <- c(mx, my)
    names(estimate) <- c("mean of x", "mean of y")
    
    stderr <- sqrt(sd_pop^2 * (1/nx + 1/ny))
    
    if (stderr < 10 * .Machine$double.eps * max(abs(mx),
                                                abs(my)))
      stop("data are essentially constant")
    zstat <- (mx - my - mu)/stderr
  }
  if (alternative == "less") {
    pval <- pnorm(zstat)
    cint <- c(-Inf, zstat + qnorm(conf.level))
  }
  else if (alternative == "greater") {
    pval <- pnorm(zstat, lower.tail = FALSE)
    cint <- c(zstat - qnorm(conf.level), Inf)
  }
  else {
    pval <- 2 * pnorm(-abs(zstat))
    alpha <- 1 - conf.level
    cint <- qnorm(1 - alpha/2)
    cint <- zstat + c(-cint, cint)
  }
  cint <- mu + cint * stderr
  names(zstat) <- "z"
  names(mu) <- if (paired || !is.null(y))
    "difference in means"
  else "mean"
  names(sd_pop) <- "Std. Dev. Population"
  attr(cint, "conf.level") <- conf.level
  rval <- list(
    statistic = zstat, parameter = sd_pop, p.value = pval,
    conf.int = cint, estimate = estimate, null.value = mu, stderr = stderr,
    alternative = alternative, method = method, data.name = dname )
  class(rval) <- "htest"
  return(rval)
}

