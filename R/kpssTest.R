
#' Kwiatkowski et al. Unit Root Test
#' 
#' Performs the KPSS unit root test, where the Null hypothesis is stationarity.
#' The test types specify as deterministic component either a constant
#' \code{"mu"} or a constant with linear trend \code{"tau"}.
#' 
#' \code{lags="short"} sets the number of lags to \eqn{\sqrt[4]{4 \times
#' (n/100)}}, whereas \code{lags="long"} sets the number of lags to
#' \eqn{\sqrt[4]{12 \times (n/100)}}. If \code{lags="nil"} is choosen, then no
#' error correction is made. Furthermore, one can specify a different number of
#' maximum lags by setting \code{use.lag} accordingly.
#' 
#' @param y Vector to be tested for a unit root.
#' @param type Type of deterministic part.
#' @param lags Maximum number of lags used for error term correction.
#' @param use.lag User specified number of lags.
#' @return An object of class \code{htest}.
#' @author Bernhard Pfaff
#' @references Kwiatkowski, D., Phillips, P.C.B., Schmidt, P. and Shin, Y.,
#' (1992), Testing the Null Hypothesis of Stationarity Against the Alternative
#' of a Unit Root: How Sure Are We That Economic Time Series Have a Unit Root?,
#' \emph{Journal of Econometrics}, \bold{54}, 159--178.
#' 
#' Download possible at: \url{https://cowles.yale.edu/}, see rubric 'Discussion
#' Papers (CFDPs)'.
#' @keywords regression
#' @examples
#' 
#' kpss.gnp <- kpssTest(AirPassengers, type="tau", lags="short")
#' summary(kpss.gnp)
#' 

#' @export 
kpssTest <- function(y, type=c("mu", "tau"), 
                     lags=c("short", "long", "nil"), use.lag=NULL){
  
  ## KPSS-Test
  y <- na.omit(as.vector(y))
  n <- length(y)
  type <- match.arg(type)
  lags <- match.arg(lags)
  if(!(is.null(use.lag))){
    lmax <- as.integer(use.lag)
    if(lmax < 0){
      warning("\nuse.lag has to be positive and integer; lags='short' used.")
    lmax <- trunc(4*(n/100)^0.25)}
  }else if(lags == "short"){
    lmax <- trunc(4*(n/100)^0.25)
  }else if(lags == "long"){
    lmax <- trunc(12*(n/100)^0.25)
  }else if(lags == "nil"){
    lmax <- 0
  }
  if(type=="mu"){
    cval <- as.matrix(t(c(0.347, 0.463, 0.574, 0.739)))
    colnames(cval) <- c("10pct", "5pct", "2.5pct", "1pct")
    rownames(cval) <- "critical values"
    res <- y - mean(y)
  }else if(type=="tau"){
    cval <- as.matrix(t(c(0.119, 0.146, 0.176, 0.216)))
    colnames(cval) <- c("10pct", "5pct", "2.5pct", "1pct")
    rownames(cval) <- "critical values"
    trend <- 1:n
    res <- residuals(lm(y ~ trend))
  }
  S <- cumsum(res)
  nominator <- sum(S^2)/n^2
  s2 <- sum(res^2)/n
  if(lmax == 0){
    denominator <- s2
  }else{
    index <- 1:lmax
    x.cov <- sapply(index, function(x) t(res[-c(1:x)])%*%res[-c((n-x+1):n)])
    bartlett <- 1-index/(lmax+1)
    denominator <- s2 + 2/n*t(bartlett)%*%x.cov
  }
  teststat <- nominator/denominator
  
  # new("ur.kpss", y=y, type=type, lag=as.integer(lmax), 
  #     teststat=as.numeric(teststat), 
  #     cval=cval, res=res , test.name="KPSS") 

  structure(
    list(
      statistic = c("KPSS" = as.numeric(teststat)),
      parameter = c(lags = as.integer(lmax)),
      critical.values = cval,
      method = paste0("KPSS Test (", 
                      if (type == "mu") "Level Stationarity" else "Trend Stationarity",
                      ")"),
      data.name = deparse(substitute(x))
    ),
    class = "htest"
  )  
  
}
