# Diagnostic Summary for Time Series Objects

Provides a compact diagnostic summary for univariate `ts` objects,
extending classical descriptive statistics with key time series
diagnostics.

## Usage

``` r
# S3 method for class 'ts'
desc(x, maxLag = 12, main = NULL, plotit = NULL, verbose = NULL, ...)

# S3 method for class 'Desc.ts'
print(x, digits = NULL, ...)

# S3 method for class 'Desc.ts'
plot(x, ...)
```

## Arguments

- x:

  a univariate object of class `"ts"`

- maxLag:

  number of lags used in the Ljung-Box test; defaults to 12

- main:

  character string, `NULL`, or `NA`, defining the main title. By default
  (`main = NULL`) the title will be composed as: (\<class(es)\>). If
  `NA`, no title is printed.

- plotit:

  logical. Should a plot be created? The plot type depends on the
  classes of the variables. Default can be defined by the option
  `plotit`, if it does not exist then it's set to `TRUE`.

- verbose:

  integer controlling verbosity of table output. One of `1` (minimal),
  `2` (default), `3` (extensive). Applies to tables only.

- ...:

  further arguments passed to methods

- digits:

  number of digits used to format numeric values

## Value

an object of class `c("Desc.ts", "Desc")` containing the computed
statistics

## Details

The function reports:

- Lag-1 autocorrelation

- Ljung-Box test for overall autocorrelation

- Augmented Dickey-Fuller (ADF) test

- KPSS test

- Linear trend estimation (slope and p-value)

- Suggested Box-Cox transformation parameter

The goal is to provide quick diagnostic guidance before model fitting
(e.g., ARIMA specification).

Stationarity is evaluated using both the Augmented Dickey-Fuller (ADF)
and KPSS tests. A combined decision rule is used: the series is
considered stationary if the ADF test rejects the null hypothesis of a
unit root (p \< 0.05) and the KPSS test does not reject the null
hypothesis of stationarity (p \> 0.05).

The Box-Cox transformation parameter is estimated using
[`boxCoxLambda()`](boxCoxLambda.md).

## References

Box, G. E. P., Jenkins, G. M., Reinsel, G. C., & Ljung, G. M. (2015).
Time Series Analysis: Forecasting and Control.

Hyndman, R. J., & Athanasopoulos, G. (2021). Forecasting: Principles and
Practice.

## See also

[stats::acf](https://rdrr.io/r/stats/acf.html),
[stats::Box.test](https://rdrr.io/r/stats/box.test.html),
[boxCoxLambda](boxCoxLambda.md),
[lumen::adfTest](https://andrisignorell.github.io/lumen/reference/adfTest.html),
[lumen::kpssTest](https://andrisignorell.github.io/lumen/reference/kpssTest.html),
[pharos::plotTimeSeries](https://andrisignorell.github.io/pharos/reference/plotTimeSeries.html)

Other desc: [`desc()`](Desc.md), [`desc.Date()`](Desc.Date.md),
[`desc.factor()`](Desc.factor.md), [`desc.nn`](Desc.nn.md),
[`desc.nq`](desc.nq.md), [`desc.numeric()`](desc.numeric.md),
[`desc.qn`](desc.qn.md), [`desc.qq`](desc.qq.md),
[`print.Desc.qq()`](desc.table.md)

## Examples

``` r
desc(AirPassengers)
#> Warning: p-value smaller than reported p-value
#> ────────────────────────────────────────────────────────────────────────────── 
#> AirPassengers (ts)
#> 
#> Warning: number of columns of result is not a multiple of vector length (arg 1)
#>  start      end  frequency          
#>    144      118          0  144  118
#>                                     
#>                                     
#>  start      end  frequency          
#> 1949-1  1960-12         12          
#>                                     

desc(Nile, maxLag = 10)
#> Warning: p-value smaller than reported p-value
#> ────────────────────────────────────────────────────────────────────────────── 
#> Nile (ts)
#> 
#> Warning: number of columns of result is not a multiple of vector length (arg 1)
#>  start     end  frequency         
#>    100      85          0  100  85
#>                                   
#>                                   
#>  start     end  frequency         
#> 1871-1  1970-1          1         
#>                                   

```
