
#' Diagnostic Summary for Time Series Objects
#'
#' Provides a compact diagnostic summary for univariate `ts` objects,
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
#' @param x a univariate object of class `"ts"`
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
#' [boxCoxLambda()].
#'
#' Missing values are allowed. The autocorrelation and the Ljung-Box test
#' keep the time structure (`na.action = na.pass`), the ADF and KPSS tests
#' and the Box-Cox parameter use the observed values only. The Box-Cox
#' parameter is `NA` unless all values are strictly positive.
#'
#' @return an object of class `c("Desc.ts", "Desc")` with, among others,
#' the components `acf1`, `ljungbox`, `adf`, `kpss` (test results),
#' `stationary` (logical, the combined rule above), `trend` (named vector
#' `slope`, `p.value`) and `boxcoxlambda`
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
                    digits = NULL,
                    ...) {

  xname <- deparse(substitute(x))

  if (NCOL(x) > 1L)
    stop("'x' must be a univariate time series; describe the columns of a ",
         "multivariate 'ts' one by one")

  total_n <- length(x)
  ok      <- !is.na(x)
  n       <- sum(ok)

  # ── Guard: all-NA or length == 0 ─────────────────────────────────────────
  if (n == 0L)
    return(.descAllNA(x, xname, main, plotit, verbose))

  # Tests that cannot deal with gaps get the observed values only; acf()
  # and Box.test() keep the time structure via na.pass, lm() drops rows.
  xv <- as.numeric(x)[ok]
  tt <- as.numeric(time(x))

  adf  <- .pBounded(adfTest(xv))
  kpss <- .pBounded(kpssTest(xv))

  trendFit <- summary(lm(as.numeric(x) ~ tt))$coefficients

  res <- list(

    meta = .descMeta(x, xname, main, plotit, verbose),

    length    = total_n,
    n         = n,
    NAs       = total_n - n,
    unique    = length(unique(xv)),
    "0s"      = sum(xv == 0),
    frequency = frequency(x),
    start     = start(x),
    end       = end(x),
    maxLag    = maxLag,
    acf1      = acf(x, plot = FALSE, na.action = na.pass)$acf[2L],
    ljungbox  = Box.test(x, lag = maxLag, type = "Ljung"),
    adf       = adf,
    kpss      = kpss,
    stationary = adf$p.value < 0.05 && kpss$p.value > 0.05,
    trend     = c(slope = trendFit[2L, 1L], p.value = trendFit[2L, 4L]),
    # Box-Cox is only defined for strictly positive data
    boxcoxlambda = if (all(xv > 0)) boxCoxLambda(xv) else NA_real_,
    digits    = digits,
    x         = x
  )

  class(res) <- c("Desc.ts", "Desc")
  res
}



#' @rdname desc.ts
#' @method print Desc.ts
#' @export
print.Desc.ts <- function(x, digits = NULL, ...) {

  .printHeader(x$meta)

  digits <- digits %||% x$digits %||% 3L

  m <- .printCore(x$length, x$n, x$NAs, x$unique)
  if (x$unique == x$n)
    m[2L, 4L] <- "= n"
  m <- cbind(m, c("0s", fm(x[["0s"]], fmt = style("abs.sty")),
                  fm(x[["0s"]] / x$length, fmt = "%", digits = 1)))
  m <- rbind(m, "",
             c("start", "end", "frequency", "", ""),
             c(paste(x$start, collapse = "-"), paste(x$end, collapse = "-"),
               x$frequency, "", ""))
  printCharMatrix(m, showRownames = FALSE)
  cat("\n")

  num  <- function(v) if (is.na(v)) "-" else fm(unname(v), digits = digits)
  pval <- function(p, bound = NA)
    if (is.na(p)) "-" else paste0(if (is.na(bound)) "" else bound,
                                  fm(unname(p), fmt = "p"))

  cat(sprintf("lag-1 autocorrelation : %s\n", num(x$acf1)))
  cat(sprintf("Ljung-Box (lag %s)     : Q = %s, p = %s\n", x$maxLag,
              num(x$ljungbox$statistic), pval(x$ljungbox$p.value)))
  cat(sprintf("ADF                   : %s, p = %s\n",
              num(x$adf$statistic),
              pval(x$adf$p.value, attr(x$adf, "pBound"))))
  cat(sprintf("KPSS                  : %s, p = %s\n",
              num(x$kpss$statistic),
              pval(x$kpss$p.value, attr(x$kpss, "pBound"))))
  cat(sprintf("stationary            : %s\n",
              if (is.na(x$stationary)) "-" else c("no", "yes")[x$stationary + 1L]))
  cat(sprintf("linear trend          : slope = %s, p = %s\n",
              num(x$trend["slope"]), pval(x$trend["p.value"])))
  cat(sprintf("Box-Cox lambda        : %s\n\n", num(x$boxcoxlambda)))

  .plotIfRequested(x)
}


#' @rdname desc.ts
#' @export
plot.Desc.ts <- function(x, ...){
  pharos::plotTimeSeries(x$x, ...)
}


# == internal helper functions ================================================

# ADF and KPSS p-values are interpolated in a table and capped at its ends;
# the tests announce the cap with a warning ("p-value smaller/greater than
# reported p-value"). For desc() that is information, not a problem - every
# clearly (non-)stationary series would warn. The warning is muffled and the
# cap kept as attribute "pBound" ("<" or ">"), which print() puts in front
# of the p-value. Any other warning passes through unchanged.
.pBounded <- function(expr) {
  bound <- NA_character_
  res <- withCallingHandlers(expr, warning = function(w) {
    msg <- conditionMessage(w)
    if (grepl("p-value (smaller|greater) than", msg)) {
      bound <<- if (grepl("smaller", msg, fixed = TRUE)) "<" else ">"
      invokeRestart("muffleWarning")
    }
  })
  attr(res, "pBound") <- bound
  res
}
