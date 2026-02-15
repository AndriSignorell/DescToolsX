
#' Diagnostic Summary for Time Series Objects
#'
#' Provides a compact diagnostic summary for univariate \code{ts} objects,
#' extending classical descriptive statistics with key time series diagnostics.
#'
#' The function reports:
#' \itemize{
#'   \item Lag-1 autocorrelation
#'   \item Ljung-Box test for overall autocorrelation
#'   \item Augmented Dickey-Fuller (ADF) test
#'   \item KPSS test
#'   \item Linear trend estimation (slope and p-value)
#'   \item Suggested Box-Cox transformation parameter
#' }
#'
#' The goal is to provide quick diagnostic guidance before model fitting
#' (e.g., ARIMA specification).
#'
#' @name Timeseries
#' @param x A univariate object of class \code{"ts"}.
#' @param lag.lb Integer. Number of lags used in the Ljung-Box test.
#'   Default is 12.
#'
#' @details
#' Stationarity is evaluated using both the Augmented Dickey-Fuller (ADF)
#' and KPSS tests. A combined decision rule is used:
#' the series is considered stationary if the ADF test rejects the null
#' hypothesis of a unit root (p < 0.05) and the KPSS test does not reject
#' the null hypothesis of stationarity (p > 0.05).
#'
#' The Box-Cox transformation parameter is estimated using
#' \code{\link{boxCoxLambda}()}.
#'
#' @return
#' The function prints a structured diagnostic summary to the console.
#' Invisibly returns a named list containing all computed statistics.
#'
#' @author
#' Andri Signorell <andri@@signorell.net>
#'
#' @references
#' Box, G. E. P., Jenkins, G. M., Reinsel, G. C., & Ljung, G. M. (2015).
#' Time Series Analysis: Forecasting and Control.
#'
#' Hyndman, R. J., & Athanasopoulos, G. (2021).
#' Forecasting: Principles and Practice.
#'
#' @examples
#' Desc(AirPassengers)
#' Desc(Nile, lag.lb = 10)
#'
#' @importFrom stats acf Box.test lm coef time
#' @importFrom utils head
#' @seealso \code{\link[stats]{acf}},
#'   \code{\link[stats]{Box.test}},
#'   \code{\link[forecast]{BoxCox.lambda}},
#'   \code{\link[tseries]{adf.test}},
#'   \code{\link[tseries]{kpss.test}}
#'
#' @export



#' @rdname Desc
#' @method Desc ts
#' @export
Desc.ts <- function(x, 
                    lag.lb = 12,
                    main = NULL,
                    plotit = NULL,
                    verbose = NULL,
                    ...) {
  
  
  # # 1️⃣ Lag-1 ACF
  # acf1 <- acf(x, plot = FALSE)$acf[2]
  # cat("Lag-1 Autocorrelation:", round(acf1, 3), "\n\n")
  # 
  # # 2️⃣ Ljung-Box
  # lb <- Box.test(x, lag = lag.lb, type = "Ljung")
  # cat("Ljung-Box Test (lag =", lag.lb, ")\n")
  # cat("  Statistic:", round(lb$statistic, 3), "\n")
  # cat("  p-value :", round(lb$p.value, 4), "\n\n")
  # 
  # # 3️⃣ Stationarity Tests
  # if(requireNamespace("tseries", quietly = TRUE)) {
  #   adf  <- tseries::adf.test(x)
  #   kpss <- tseries::kpss.test(x)
  #   
  #   cat("ADF Test p-value :", round(adf$p.value, 4), "\n")
  #   cat("KPSS Test p-value:", round(kpss$p.value, 4), "\n")
  #   
  #   stationary <- (adf$p.value < 0.05) & (kpss$p.value > 0.05)
  #   cat("Stationary (combined decision):", stationary, "\n\n")
  # } else {
  #   cat("Install package 'tseries' for stationarity tests.\n\n")
  # }
  # 
  # # 4️⃣ Linear Trend
  # t <- time(x)
  # fit <- lm(x ~ t)
  # slope <- coef(fit)[2]
  # pval  <- summary(fit)$coefficients[2,4]
  # 
  # cat("Linear Trend\n")
  # cat("  Slope  :", round(slope, 4), "\n")
  # cat("  p-value:", round(pval, 4), "\n\n")
  # 
  # # 5️⃣ BoxCox Lambda
  # if(requireNamespace("forecast", quietly = TRUE)) {
  #   lambda <- forecast::BoxCox.lambda(x)
  #   cat("Suggested BoxCox Lambda:", round(lambda, 3), "\n")
  #   
  #   if(abs(lambda) < 0.15)
  #     cat("  → Log transformation recommended\n")
  # } else {
  #   cat("Install package 'forecast' for BoxCox suggestion.\n")
  # }
  

  res <- list(
    
    meta = .descMeta(x, deparse(substitute(x)), main, plotit, verbose),
    
    n = length(x),
    unique = length(unique(x)),
    "0s" = sum(x == 0),
    frequency = frequency(x),
    start = start(x),
    end = end(x),
    acf1 = acf(x, plot = FALSE)$acf[2],
    ljungbox=Box.test(x, lag = lag.lb, type = "Ljung"),
    adf = tseries::adf.test(x),
    kpss = tseries::kpss.test(x), 
    fit <- lm(x ~ time(x)),
    boxcoxlambda = boxCoxLambda(x),
    x = x
  )
  class(res) <- c("Desc.ts", "Desc")
  
  return(res)
}



#' @rdname Desc
#' @method print Desc.ts
#' @export
print.Desc.ts <- function(x, digits = NULL, ...) {
  
  .printHeader(x$meta)
  
  # x["nperc"] <- fm(x[["n"]] / x[["length"]], fmt = "%", digits = 1)
  # x["naperc"] <- fm(x[["NAs"]] / x[["length"]], fmt = "%", digits = 1)
  # x["zeroperc"] <- fm(x[["0s"]] / x[["length"]], fmt = "%", digits = 1)
  # 
  # x[c("length", "n", "NAs", "unique", "0s")] <-
  #   lapply(x[c("length", "n", "NAs", "unique", "0s")],
  #          fm,
  #          fmt = "abs.sty"
  #   )
  
  lst <- list(
    l1 = unlist(x[c("length", "n", "NAs", "unique", "0s")]),
    l2 = c("", x[["nperc"]], x[["naperc"]], "", x[["zeroperc"]]),
    l3 = c(
      start = paste(x$start, collapse = "-"),
      end = paste(x$end, collapse = "-"),
      frequency = x$frequency, "", ""
    )
  )
  
  width <- max(c(
    unlist(lapply(lst, nchar)),
    unlist(lapply(lapply(lst, names), nchar))
  ), na.rm = TRUE)
  if (x$unique == x$n) {
    lst$l1["unique"] <- "= n"
  }
  
  m <- rbind(lst$l1, lst$l2, "", names(lst$l3), lst$l3, "")
  .print.charmatrix(m)
  
  
  if(x$meta$plotit)
    plot(x, main=x$meta$main)
  
  
}



