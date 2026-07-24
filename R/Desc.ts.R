
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
#' @name desc.ts
#' 
#' @inheritParams desc
#' @param x a univariate object of class \code{"ts"}
#' @param maxLag number of lags used in the Ljung-Box test; defaults to 12
#' @param digits number of digits used to format numeric values
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
#' @return an object of class \code{c("Desc.ts", "Desc")} containing the
#' computed statistics
#'
#' @references
#' Box, G. E. P., Jenkins, G. M., Reinsel, G. C., & Ljung, G. M. (2015).
#' Time Series Analysis: Forecasting and Control.
#'
#' Hyndman, R. J., & Athanasopoulos, G. (2021).
#' Forecasting: Principles and Practice.
#'
#' @examples
#' desc(AirPassengers)
#' desc(Nile, maxLag = 10)
#'
#' @seealso [stats::acf], [stats::Box.test], [boxCoxLambda],
#'   [lumen::adfTest],
#'   [lumen::kpssTest], 
#'   [pharos::plotTimeSeries]
#'
#' @family desc
#' @concept data-description
#' @concept descriptive-statistics
#' @concept time-series
#' 
#'
#' @rdname desc.ts
#' @method desc ts
#' @export
desc.ts <- function(x, 
                    maxLag = 12,
                    main = NULL,
                    plotit = NULL,
                    verbose = NULL,
                    ...) {
  
  
  total_n <- length(x)    # total n
  ok <- !is.na(x)         # non NAs
  n <- sum(ok)            # valid n
  
  
  # ── Guard: all-NA oder length == 0 ─────────────────────────────────────────
  if (n == 0L)
    return(.descAllNA(x, deparse(substitute(x)), main, plotit, verbose))
  
  
  res <- list(
    
    meta = .descMeta(x, deparse(substitute(x)), main, plotit, verbose),
    
    n = length(x),
    unique = length(unique(x)),
    "0s" = sum(x == 0),
    frequency = frequency(x),
    start = start(x),
    end = end(x),
    acf1 = acf(x, plot = FALSE)$acf[2],
    ljungbox=Box.test(x, lag = maxLag, type = "Ljung"),
    adf = adfTest(x),
    kpss = kpssTest(x), 
    fit <- lm(x ~ time(x)),
    boxcoxlambda = boxCoxLambda(x),
    x = x
  )
  class(res) <- c("Desc.ts", "Desc")
  
  return(res)
}



#' @rdname desc.ts
#' @method print Desc.ts
#' @export
print.Desc.ts <- function(x, digits = NULL, ...) {
  
  .printHeader(x$meta)
 
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
  printCharMatrix(m, showRownames = FALSE)  
  
  if(x$meta$plotit)
    plot(x, main=x$meta$main)
  
  
}


#' @rdname desc.ts
#' @export
plot.Desc.ts <- function(x, ...){
  plotTimeSeries(x$x, ...)
}

