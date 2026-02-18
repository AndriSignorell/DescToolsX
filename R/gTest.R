
#' G-Test for Count Data
#' 
#' \code{gTest} performs chi-squared contingency table tests and
#' goodness-of-fit tests.
#' 
#' The G-test is also called "Likelihood Ratio Test" and is asymptotically
#' equivalent to the Pearson ChiSquare-test but not usually used when analyzing
#' 2x2 tables. It is used in logistic regression and loglinear modeling which
#' involves contingency tables. The G-test is also reported in the standard
#' summary of \code{Desc} for tables.
#' 
#' If \code{x} is a matrix with one row or column, or if \code{x} is a vector
#' and \code{y} is not given, then a \emph{goodness-of-fit test} is performed
#' (\code{x} is treated as a one-dimensional contingency table).  The entries
#' of \code{x} must be non-negative integers.  In this case, the hypothesis
#' tested is whether the population probabilities equal those in \code{p}, or
#' are all equal if \code{p} is not given.
#' 
#' If \code{x} is a matrix with at least two rows and columns, it is taken as a
#' two-dimensional contingency table: the entries of \code{x} must be
#' non-negative integers.  Otherwise, \code{x} and \code{y} must be vectors or
#' factors of the same length; cases with missing values are removed, the
#' objects are coerced to factors, and the contingency table is computed from
#' these.  Then G-test is performed on the null hypothesis that the joint
#' distribution of the cell counts in a 2-dimensional contingency table is the
#' product of the row and column marginals.
#' 
#' @param x a numeric vector or matrix. \code{x} and \code{y} can also both be
#' factors.
#' @param y a numeric vector; ignored if \code{x} is a matrix.  If \code{x} is
#' a factor, \code{y} should be a factor of the same length.
#' @param correct one out of \code{"none"} (default), \code{"williams"},
#' \code{"yates"} . See Details.
#' @param p a vector of probabilities of the same length of \code{x}.  An error
#' is given if any entry of \code{p} is negative.
#' @param rescale.p a logical scalar; if \code{TRUE} then p is rescaled (if
#' necessary) to sum to 1. If rescale.p is \code{FALSE}, and p does not sum to
#' 1, an error is given.

#' @return A list with class \code{"htest"} containing the following
#' components: \item{statistic}{the value the chi-squared test statistic.}
#' \item{parameter}{the degrees of freedom of the approximate chi-squared
#' distribution of the test statistic, \code{NA} if the p-value is computed by
#' Monte Carlo simulation.} \item{p.value}{the p-value for the test.}
#' \item{method}{a character string indicating the type of test performed, and
#' whether Monte Carlo simulation or continuity correction was used.}
#' \item{data.name}{a character string giving the name(s) of the data.}
#' \item{observed}{the observed counts.} \item{expected}{the expected counts
#' under the null hypothesis.}

#' @author Pete Hurd <phurd@@ualberta.ca>, Andri Signorell
#' <andri@@signorell.net> (tiny tweaks)

#' @seealso \code{\link{chisq.test}}.
#' @references Hope, A. C. A. (1968) A simplified Monte Carlo significance test
#' procedure.  \emph{J. Roy, Statist. Soc. B} \bold{30}, 582--598.
#' 
#' Patefield, W. M. (1981) Algorithm AS159.  An efficient method of generating
#' r x c tables with given row and column totals.  \emph{Applied Statistics}
#' \bold{30}, 91--97.
#' 
#' Agresti, A. (2007) \emph{An Introduction to Categorical Data Analysis, 2nd
#' ed.}, New York: John Wiley & Sons.  Page 38.
#' 
#' Sokal, R. R., F. J. Rohlf (2012) \emph{Biometry: the principles and practice
#' of statistics in biological research}. 4th edition. W. H. Freeman and Co.:
#' New York. 937 pp.
#' 
#' @family topic.contingency-tests
#' @concept association
#' 
#' @examples
#' 
#' 
#' ## From Agresti(2007) p.39
#' M <- as.table(rbind(c(762, 327, 468), c(484,239,477)))
#' dimnames(M) <- list(gender=c("M","F"),
#'                     party=c("Democrat","Independent", "Republican"))
#' 
#' (Xsq <- gTest(M))   # Prints test summary
#' 
#' Xsq$observed        # observed counts (same as M)
#' Xsq$expected        # expected counts under the null
#' 
#' 
#' ## Testing for population probabilities
#' ## Case A. Tabulated data
#' x <- c(A = 20, B = 15, C = 25)
#' gTest(x)
#' gTest(as.table(x))             # the same
#' x <- c(89,37,30,28,2)
#' p <- c(40,20,20,15,5)
#' try(
#' gTest(x, p = p)                # gives an error
#' )
#' # works
#' p <- c(0.40,0.20,0.20,0.19,0.01)
#' # Expected count in category 5
#' # is 1.86 < 5 ==> chi square approx.
#' gTest(x, p = p)                # maybe doubtful, but is ok!
#' 
#' ## Case B. Raw data
#' x <- trunc(5 * runif(100))
#' gTest(table(x))                # NOT 'gTest(x)'!
#' 


#' @export
gTest <- function(x, y = NULL, correct=c("none", "williams", "yates"), 
                  p = rep(1/length(x), length(x)), rescale.p = FALSE) {
  
  
  # Log-likelihood tests of independence & goodness of fit
  # Does Williams' and Yates' correction
  # does Monte Carlo simulation of p-values, via gTestsim.c
  #
  # G & q calculation from Sokal & Rohlf (1995) Biometry 3rd ed.
  # TOI Yates' correction taken from Mike Camann's 2x2 G-test fn.
  # GOF Yates' correction as described in Zar (2000)
  # more stuff taken from ctest's chisq.test()
  #
  # ToDo:
  # 1) Beautify
  # 2) Add warnings for violations
  # 3) Make appropriate corrections happen by default
  #
  # V3.3 Pete Hurd Sept 29 2001. phurd@ualberta.ca
  
  
  DNAME <- deparse(substitute(x))
  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.matrix(x)) {
    if (min(dim(x)) == 1)
      x <- as.vector(x)
  }
  if (!is.matrix(x) && !is.null(y)) {
    if (length(x) != length(y))
      stop("x and y must have the same length")
    DNAME <- paste(DNAME, "and", deparse(substitute(y)))
    OK <- complete.cases(x, y)
    x <- as.factor(x[OK])
    y <- as.factor(y[OK])
    if ((nlevels(x) < 2) || (nlevels(y) < 2))
      stop("x and y must have at least 2 levels")
    x <- table(x, y)
  }
  if (any(x < 0) || any(is.na(x)))
    stop("all entries of x must be nonnegative and finite")
  if ((n <- sum(x)) == 0)
    stop("at least one entry of x must be positive")
  
  correct <- match.arg(correct)
  
  #If x is matrix, do test of independence
  if (is.matrix(x)) {
    #Test of Independence
    nrows<-nrow(x)
    ncols<-ncol(x)
    if (correct=="yates"){ # Do Yates' correction?
      if(dim(x)[1]!=2 || dim(x)[2]!=2) # check for 2x2 matrix
        stop("Yates' correction requires a 2 x 2 matrix")
      if((x[1,1]*x[2,2])-(x[1,2]*x[2,1]) > 0)
      {
        #         x[1,1] <- x[1,1] - 0.5
        #         x[2,2] <- x[2,2] - 0.5
        #         x[1,2] <- x[1,2] + 0.5
        #         x[2,1] <- x[2,1] + 0.5
        #   this can be done quicker: 14.5.2015 AS
        x <- x + 0.5
        diag(x) <- diag(x) - 1
        
      } else {
        
        x <- x - 0.5
        diag(x) <- diag(x) + 1
        
        #         x[1,1] <- x[1,1] + 0.5
        #         x[2,2] <- x[2,2] + 0.5
        #         x[1,2] <- x[1,2] - 0.5
        #         x[2,1] <- x[2,1] - 0.5
      }
    }
    
    sr <- apply(x,1,sum)
    sc <- apply(x,2,sum)
    E <- outer(sr,sc, "*")/n
    # are we doing a monte-carlo?
    # no monte carlo GOF?
    #     if (simulate.p.value){
    #       METHOD <- paste("Log likelihood ratio (G-test) test of independence\n\t with simulated p-value based on", B, "replicates")
    #       tmp <- .C("gTestsim", as.integer(nrows), as.integer(ncols),
    #                 as.integer(sr), as.integer(sc), as.integer(n), as.integer(B),
    #                 as.double(E), integer(nrows * ncols), double(n+1),
    #                 integer(ncols), results=double(B), PACKAGE= "ctest")
    #       g <- 0
    #       for (i in 1:nrows){
    #         for (j in 1:ncols){
    #           if (x[i,j] != 0) g <- g + x[i,j] * log(x[i,j]/E[i,j])
    #         }
    #       }
    #       STATISTIC <- G <- 2 * g
    #       PARAMETER <- NA
    #       PVAL <- sum(tmp$results >= STATISTIC)/B
    #     }
    #     else {
    # no monte-carlo
    # calculate G
    g <- 0
    for (i in 1:nrows){
      for (j in 1:ncols){
        if (x[i,j] != 0) g <- g + x[i,j] * log(x[i,j]/E[i,j])
      }
      # }
      q <- 1
      if (correct=="williams"){ # Do Williams' correction
        row.tot <- col.tot <- 0
        for (i in 1:nrows){ row.tot <- row.tot + 1/(sum(x[i,])) }
        for (j in 1:ncols){ col.tot <- col.tot + 1/(sum(x[,j])) }
        q <- 1+ ((n*row.tot-1)*(n*col.tot-1))/(6*n*(ncols-1)*(nrows-1))
      }
      STATISTIC <- G <- 2 * g / q
      PARAMETER <- (nrow(x)-1)*(ncol(x)-1)
      PVAL <- 1-pchisq(STATISTIC,df=PARAMETER)
      if(correct=="none")
        METHOD <- "Log likelihood ratio (G-test) test of independence without correction"
      if(correct=="williams")
        METHOD <- "Log likelihood ratio (G-test) test of independence with Williams' correction"
      if(correct=="yates")
        METHOD <- "Log likelihood ratio (G-test) test of independence with Yates' correction"
    }
  }
  else {
    
    
    # x is not a matrix, so we do Goodness of Fit
    METHOD <- "Log likelihood ratio (G-test) goodness of fit test"
    
    if (length(dim(x)) > 2L) 
      stop("invalid 'x'")
    if (length(x) == 1L)
      stop("x must at least have 2 elements")
    if (length(x) != length(p))
      stop("'x' and 'p' must have the same number of elements")
    if (any(p < 0)) 
      stop("probabilities must be non-negative.")
    if (abs(sum(p) - 1) > sqrt(.Machine$double.eps)) {
      if (rescale.p) 
        p <- p/sum(p)
      else stop("probabilities must sum to 1.")
    }
    
    E <- n * p
    
    if (correct=="yates"){ # Do Yates' correction
      if(length(x)!=2)
        stop("Yates' correction requires 2 data values")
      if ( (x[1]-E[1]) > 0.25) {
        x[1] <- x[1]-0.5
        x[2] <- x[2]+0.5
      }
      else if ( (E[1]-x[1]) > 0.25){
        x[1] <- x[1]+0.5
        x[2] <- x[2]-0.5
      }
    }
    names(E) <- names(x)
    g <- 0
    for (i in 1:length(x)){
      if (x[i] != 0) g <- g + x[i] * log(x[i]/E[i])
    }
    q <- 1
    if (correct=="williams"){ # Do Williams' correction
      q <- 1+(length(x)+1)/(6*n)
    }
    STATISTIC <- G <- 2*g/q
    PARAMETER <- length(x) - 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
  }
  names(STATISTIC) <- "G"
  names(PARAMETER) <- "X-squared df"
  names(PVAL) <- "p.value"
  structure(list(statistic=STATISTIC,parameter=PARAMETER,p.value=PVAL,
                 method=METHOD,data.name=DNAME, observed=x, expected=E),
            class="htest")
}

