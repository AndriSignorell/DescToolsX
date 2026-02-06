
#' (Robust) Jarque Bera Test
#' 
#' This function performs the Jarque-Bera tests of normality either the robust
#' or the classical way.
#' 
#' The test is based on a joint statistic using skewness and kurtosis
#' coefficients. The robust Jarque-Bera (RJB) version of utilizes the robust
#' standard deviation (namely the mean absolute deviation from the median, as
#' provided e. g. by \code{\link{meanAD}(x, FUN=median)}) to estimate sample
#' kurtosis and skewness. For more details see Gel and Gastwirth (2006). \cr
#' Setting \code{robust} to \code{FALSE} will perform the original Jarque-Bera
#' test (see Jarque, C. and Bera, A (1980)).
#' 
#' @param x a numeric vector of data values.
#' @param robust defines, whether the robust version should be used.  Default
#' is \code{TRUE}.
#' @param method a character string out of \code{chisq} or \code{mc},
#' specifying how the critical values should be obtained. Default is
#' approximated by the chisq-distribution or empirically via Monte Carlo.
#' @param N number of Monte Carlo simulations for the empirical critical values
#' @param na.rm defines if \code{NAs} should be omitted. Default is
#' \code{FALSE}.
#' @return A list with class \code{htest} containing the following components:
#' \item{statistic}{the value of the test statistic.} \item{parameter}{the
#' degrees of freedom.} \item{p.value}{the p-value of the test.}
#' \item{method}{type of test was performed.} \item{data.name}{a character
#' string giving the name of the data.}
#' @note This function is melted from the \code{jarque.bera.test} (in
#' \code{tseries} package) and the \code{rjb.test} from the package
#' \code{lawstat}.
#' @author W. Wallace Hui, Yulia R. Gel, Joseph L. Gastwirth, Weiwen Miao
#' @seealso Alternative tests for normality as \code{\link{shapiro.test}},
#' \code{\link{andersonDarlingTest}}, \code{\link{cramerVonMisesTest}},
#' \code{\link{lillieTest}}, \code{\link{pearsonTest}},
#' \code{\link{shapiroFranciaTest}}
#' 
#' \code{\link{qqnorm}}, \code{\link{qqline}} for producing a normal
#' quantile-quantile plot
#' 
#' @references Gastwirth, J. L.(1982) \emph{Statistical Properties of A Measure
#' of Tax Assessment Uniformity}, Journal of Statistical Planning and Inference
#' 6, 1-12.\cr
#' 
#' Gel, Y. R. and Gastwirth, J. L. (2008) \emph{A robust modification of the
#' Jarque-Bera test of normality}, Economics Letters 99, 30-32.\cr
#' 
#' Jarque, C. and Bera, A. (1980) \emph{Efficient tests for normality,
#' homoscedasticity and serial independence of regression residuals}, Economics
#' Letters 6, 255-259.
#' @keywords htest
#' @examples
#' 
#' x <- rnorm(100)    # null hypothesis
#' jarqueBeraTest(x)
#' 
#' x <- runif(100)    # alternative hypothesis
#' jarqueBeraTest(x, robust=TRUE)
#' 


#' @export
jarqueBeraTest <- function (x, robust=TRUE, 
                            method=c("chisq", "mc"), N=0, na.rm=FALSE) {
  
  
  # ***********************************
  # Tests aus library(tseries)
  #
  # jarqueBeraTest <- function(x, robust=TRUE, na.rm=FALSE) {
  #
  #   # Author: Adrian Trapletti
  #
  #   if(NCOL(x) > 1)
  #       stop("x is not a vector or univariate time series")
  #
  #   if(na.rm) x <- na.omit(x)
  #
  #   DNAME <- deparse(substitute(x))
  #   n <- length(x)
  #   m1 <- sum(x)/n
  #   m2 <- sum((x-m1)^2)/n
  #   m3 <- sum((x-m1)^3)/n
  #   m4 <- sum((x-m1)^4)/n
  #   b1 <- (m3/m2^(3/2))^2
  #   b2 <- (m4/m2^2)
  #   STATISTIC <- n * b1 / 6 + n * (b2 - 3)^2 / 24
  #   names(STATISTIC) <- "X-squared"
  #   PARAMETER <- 2
  #   names(PARAMETER) <- "df"
  #   PVAL <- 1 - pchisq(STATISTIC,df = 2)
  #   METHOD <- "Jarque Bera Test"
  #   structure(list(statistic = STATISTIC,
  #                  parameter = PARAMETER,
  #                  p.value = PVAL,
  #                  method = METHOD,
  #                  data.name = DNAME),
  #             class = "htest")
  # }
  #
  #
  
  
  method <- match.arg(method)
  
  if (NCOL(x) > 1){ stop("x is not a vector or univariate time series") }
  if(na.rm) x <- na.omit(x)
  
  if ((method == "mc") & (N==0)) {
    stop("number of Monte Carlo simulations N should be provided for the empirical critical values")
  }
  
  DNAME <- deparse(substitute(x))
  
  ## Calculate the first 4 central moments
  n <- length(x)
  m1 <- sum(x)/n
  m2 <- sum((x - m1)^2)/n
  m3 <- sum((x - m1)^3)/n
  m4 <- sum((x - m1)^4)/n
  
  ## User can choose the Standard Jarque Bera Test or Robust Jarque Bera Test
  ## Robust Jarque Bera Test is default
  if(!robust) {
    b1 <- (m3/m2^(3/2))^2;
    b2 <- (m4/m2^2);
    statistic <- n * b1/6 + n * (b2 - 3)^2/24
    
  } else {
    J <- sqrt(pi/2) * mean(abs(x-median(x)))
    J2 <- J^2
    b1 <- (m3/(J2)^(3/2))^2
    b2 <- (m4/(J2)^2)
    vk<-64/n
    vs<-6/n
    ek<-3
    statistic <- b1/vs + (b2 - ek)^2/vk
    
  }
  
  if(method == "mc"){
    if(!robust) {
      ## computes empirical critical values for the JB statistic
      
      jb<-double(N)
      
      for (k in 1:N) {
        e <- rnorm(length(x), mean=0, sd = sqrt(1))
        m1 <- sum(e)/n
        m2 <- sum((e - m1)^2)/n
        m3 <- sum((e - m1)^3)/n
        m4 <- sum((e - m1)^4)/n
        b1 <- (m3/m2^(3/2))^2
        b2 <- (m4/m2^2)
        vk <- 24/n
        vs <- 6/n
        ek <- 3
        jb[k] <- b1/vs + (b2 - ek)^2/vk
      }
      
      y <- sort(jb)
      
      if (statistic >= max(y)) {
        p.value <- 0
        
      } else if (statistic<=min(y)) {
        p.value <- 1
        
      } else {
        bn <- which(y==min(y[I(y>=statistic)]))
        an <- which(y==max(y[I(y<statistic)]))
        a <- max(y[I(y<statistic)])
        b <- min(y[I(y>=statistic)])
        pa <- (an - 1) / (N - 1)
        pb <- (bn - 1) / (N - 1)
        alpha <- (statistic-a)/(b-a)
        p.value <- 1-alpha*pb-(1-alpha)*pa
      }
      
    } else {
      ## computes empirical critical values for the RJB statistic
      rjb <- double(N)
      
      for (k in 1:N) {
        e <- rnorm(length(x), mean=0, sd = sqrt(1))
        J <- sqrt(pi/2)*mean(abs(e-median(e)))
        J2 <- J^2
        m1 <- sum(e)/n
        m2 <- sum((e - m1)^2)/n
        m3 <- sum((e - m1)^3)/n
        m4 <- sum((e - m1)^4)/n
        b1 <- (m3/(J2)^(3/2))^2
        b2 <- (m4/(J2)^2)
        vk <- 64/n
        vs <- 6/n
        ek <- 3
        rjb[k] <- b1/vs + (b2 - ek)^2/vk
      }
      
      y <- sort(rjb)
      
      if (statistic >= max(y)) {
        p.value <- 0
        
      } else if (statistic <= min(y)) {
        p.value <- 1
        
      } else {
        bn <- which(y==min(y[I(y>=statistic)]))
        an <- which(y==max(y[I(y<statistic)]))
        a <- max(y[I(y<statistic)])
        b <- min(y[I(y>=statistic)])
        pa <- (an - 1) / (N - 1)
        pb <- (bn - 1) / (N - 1)
        alpha <- (statistic-a)/(b-a)
        p.value <- 1-alpha*pb-(1-alpha)*pa
      }
    }
    
  } else {
    p.value <- 1 - pchisq(statistic, df = 2)
  }
  
  METHOD <- ifelse(!robust, "Jarque Bera Test", "Robust Jarque Bera Test")
  STATISTIC=statistic
  names(STATISTIC) <- "X-squared"
  PARAMETER <- 2
  names(PARAMETER) <- "df"
  
  structure(list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = p.value, method = METHOD, data.name = DNAME),
            class = "htest")
  
}



