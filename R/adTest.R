

#' Anderson-Darling Test of Goodness-of-Fit
#' 
#' Performs the Anderson-Darling test of goodness-of-fit to a specified
#' continuous univariate probability distribution.
#' 
#' This command performs the Anderson-Darling test of goodness-of-fit to the
#' distribution specified by the argument \code{null}. It is assumed that the
#' values in \code{x} are independent and identically distributed random
#' values, with some cumulative distribution function \eqn{F}.  The null
#' hypothesis is that \eqn{F} is the function specified by the argument
#' \code{null}, while the alternative hypothesis is that \eqn{F} is some other
#' function.
#' 
#' By default, the test assumes that all the parameters of the null
#' distribution are known in advance (a \emph{simple} null hypothesis).  This
#' test does not account for the effect of estimating the parameters.
#' 
#' If the parameters of the distribution were estimated (that is, if they were
#' calculated from the same data \code{x}), then this should be indicated by
#' setting the argument \code{estimated=TRUE}.  The test will then use the
#' method of Braun (1980) to adjust for the effect of parameter estimation.
#' 
#' Note that Braun's method involves randomly dividing the data into two
#' equally-sized subsets, so the \eqn{p}-value is not exactly the same if the
#' test is repeated.  This technique is expected to work well when the number
#' of observations in \code{x} is large.
#' 
#' @param x Numeric vector of data values.
#' @param null A function, or a character string giving the name of a function,
#' to compute the cumulative distribution function for the null distribution.
#' @param \dots Additional arguments for the cumulative distribution function.
#' @param estimated Logical value indicating whether the parameters of the
#' distribution were estimated using the data \code{x} (composite null
#' hypothesis), or were fixed in advance (simple null hypothesis, the default).
#' @param nullname Optional character string describing the null distribution.
#' The default is \code{"uniform distribution"}.
#' @return An object of class \code{"htest"} representing the result of the
#' hypothesis test.
#' @author Original C code by George Marsaglia and John Marsaglia.  \R
#' interface by Adrian Baddeley.
#' @seealso \code{\link{pAD}} for the null distribution of the test statistic.
#' @references Anderson, T.W. and Darling, D.A. (1952) Asymptotic theory of
#' certain 'goodness-of-fit' criteria based on stochastic processes.
#' \emph{Annals of Mathematical Statistics} \bold{23}, 193--212.
#' 
#' Anderson, T.W. and Darling, D.A. (1954) A test of goodness of fit.
#' \emph{Journal of the American Statistical Association} \bold{49}, 765--769.
#' 
#' Braun, H. (1980) A simple method for testing goodness-of-fit in the presence
#' of nuisance parameters.  \emph{Journal of the Royal Statistical Society}
#' \bold{42}, 53--63.
#' 
#' Marsaglia, G. and Marsaglia, J. (2004) Evaluating the Anderson-Darling
#' Distribution.  \emph{Journal of Statistical Software} \bold{9} (2), 1--5.
#' February 2004.  \url{http://www.jstatsoft.org/v09/i02}
#' @keywords htest
#' @examples
#' 
#' x <- rnorm(10, mean=2, sd=1)
#' andersonDarlingTest(x, "pnorm", mean=2, sd=1)
#' andersonDarlingTest(x, "pnorm", mean=mean(x), sd=sd(x), estimated=TRUE)
#'

 
#' @export
andersonDarlingTest <- function(x, null="punif", ..., estimated=FALSE, nullname) {
  
  ##
  ## andarl.R
  ##
  ##  Anderson-Darling test and null distribution
  ##
  ## $Revision: 1.10 $ $Date: 2018/06/06 08:25:51 $
  ##
  
  
  xname <- deparse(substitute(x))
  nulltext <- deparse(substitute(null))
  if(is.character(null)) nulltext <- null
  if(missing(nullname) || is.null(nullname)) {
    reco <- recogniseCdf(nulltext)
    nullname <- if(!is.null(reco)) reco else 
      paste("distribution", sQuote(nulltext))
  }
  stopifnot(is.numeric(x))
  x <- as.vector(x)
  n <- length(x)
  F0 <- getCdf(null)
  U <- F0(x, ...)
  if(any(U < 0 | U > 1))
    stop("null distribution function returned values outside [0,1]")
  
  # perform test
  if(!estimated || n <= 4) {
    # simple null hypothesis
    z <- simpleADtest(U)
    ADJUST <- NULL
  } else {  
    # composite - use Braun (1980)
    m <- round(sqrt(n))
    z <- braun(U, simpleADtest, m=m)
    ADJUST <- paste("Braun's adjustment using", m, "groups")
  }
  PVAL             <- z$pvalue
  STATISTIC        <- z$statistic
  names(STATISTIC) <- z$statname
  
  # dress up
  METHOD <- c("Anderson-Darling test of goodness-of-fit",
              ADJUST,
              paste("Null hypothesis:", nullname))
  extras <- list(...)
  parnames <- intersect(names(extras), names(formals(F0)))
  if(length(parnames) > 0) {
    pars <- extras[parnames]
    pard <- character(length(parnames))
    for(i in seq_along(parnames))
      pard[i] <- paste(parnames[i], "=", paste(pars[[i]], collapse=" "))
    pard <- paste("with",
                  ngettext(length(pard), "parameter", "parameters"),
                  "  ", 
                  paste(pard, collapse=", "))
    METHOD <- c(METHOD, pard)
  }
  
  coda <- paste("Parameters assumed to",
                if(estimated) "have been estimated from data" else "be fixed")
  METHOD <- c(METHOD, coda)
  
  out <- list(statistic = STATISTIC,
              p.value = PVAL,
              method = METHOD,
              data.name = xname)
  class(out) <- "htest"
  return(out)
}



simpleADtest <- function(U) {
  ## Internal: call Marsaglia C code
  U <- sort(U)
  n <- length(U)
  z <- ADtestR(U)
  return(list(statistic=z$adstat, pvalue=z$pvalue, statname="An"))
}




# == internal helper functions ========================================

##  recog.R
##
## $Revision: 1.4 $ $Date: 2014/06/24 02:13:35 $
##

recogniseCdf <- function(s="punif") {
  if(!is.character(s) || length(s) != 1 || nchar(s) == 0) return(NULL)
  # strip off the leading 'p' if present
  root <- if(substr(s,1,1) == "p") substr(s, 2, nchar(s)) else s
  a <- switch(root,
              beta     = "beta",
              binom    = "binomial",
              birthday = "birthday coincidence",
              cauchy   = "Cauchy",
              chisq    = "chi-squared",
              exp      = "exponential",
              f        = "F",
              gamma    = "Gamma",
              geom     = "geometric",
              hyper    = "hypergeometric",
              lnorm    = "log-normal",
              logis    = "logistic",
              nbinom   = "negative binomial",
              norm     = "Normal",
              pois     = "Poisson",
              t        = "Student's t",
              tukey    = "Tukey (Studentized range)",
              unif     = "uniform",
              weibull  = "Weibull",
              NULL)
  if(!is.null(a))
    return(paste(a, "distribution"))
  b <- switch(root,
              AD     = "Anderson-Darling",
              CvM    = "Cramer-von Mises",
              wilcox = "Wilcoxon Rank Sum",
              NULL)
  if(!is.null(b))
    return(paste("null distribution of", b, "Test Statistic"))
  return(NULL)
}


getfunky <- function(fname) {
  a <- mget(fname, mode="function", ifnotfound=list(NULL), inherits=TRUE)[[1]]
  return(a)
}

getCdf <- function(s="punif", fatal=TRUE) {
  sname <- deparse(substitute(s), nlines=1L)
  if(is.function(s)) return(s)
  if(is.character(s) && length(s) == 1 && nchar(s) > 0) {
    # first try adding a leading 'p' (to catch the case s="t")
    if(substr(s,1,1) != "p") {
      f <- getfunky(paste0("p", s))
      if(is.function(f))
        return(f)
    }
    f <- getfunky(s)
    if(is.function(f))
      return(f)
  }
  if(fatal)
    stop(paste("Argument", sQuote(sname),
               "should be a function, or the name of a function"),
         call.=FALSE)
  return(NULL)
}


# braun.R
# Braun (1980) method for composite null hypothesis
#

braun <- function(U, simpletest, m) {
  n <- length(U)
  if(n < 2 * m) stop("Unsufficient data for Braun's method")
  # split data into m groups
  group <- factor(sample(seq_len(n) %% m))
  # apply the simple-null test to each subset
  zz <- by(data=U, INDICES=group, FUN=simpletest, simplify=FALSE)
  statistics <- sapply(zz, getElement, "statistic")
  pvalues    <- sapply(zz, getElement, "pvalue")
  statname   <- zz[[1]]$statname
  # combine
  statistic <- max(statistics)
  pvalue <- 1 - (1 - min(pvalues))^m
  statname <- paste0(statname, "max")
  return(list(statistic=statistic, pvalue=pvalue, statname=statname))
}
