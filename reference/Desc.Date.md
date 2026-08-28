# Descriptive statistics for calendar date variables

Computes a structured descriptive summary for objects of class `"Date"`.
The description focuses on time-axis characteristics (range, span,
coverage, quantiles) and distributional structure over weekdays and
months.

## Usage

``` r
# S3 method for class 'Date'
desc(
  x,
  main = NULL,
  plotit = NULL,
  verbose = NULL,
  wprobs = rep(1/7, 7),
  mprobs = rep(1/12, 12),
  ...
)
```

## Arguments

- x:

  a dichotomous numeric, integer, factor, character, or logical vector

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

- wprobs:

  numeric vector of length 7 specifying expected probabilities for
  weekdays (Monday to Sunday). The default is a uniform distribution
  `rep(1/7, 7)`.

- mprobs:

  numeric vector of length 12 specifying expected probabilities for
  months (January to December). If `NULL` (default), probabilities
  proportional to the number of days per month in a non-leap year are
  used.

- ...:

  further arguments passed to methods

## Value

an object of class `c("Desc.Date", "Desc")` with components:

- `core`:

  time-axis statistics

- `weekday`:

  observed and expected weekday counts, standardized residuals, and
  p-value

- `month`:

  observed and expected month counts, standardized residuals, and
  p-value

- `sentinel`:

  heuristic data-quality diagnostics

- `meta`:

  metadata

## Details

In addition to core time-axis statistics, observed and expected
frequencies for weekdays and months are calculated together with
standardized residuals and chi-square p-values. The function also
performs heuristic detection of suspicious sentinel dates (e.g., extreme
future or implausibly early values) to highlight potential data-quality
issues.

The core time-axis summary includes:

- Number of observations and missing values

- Minimum and maximum date

- Span in days (`max - min`)

- Number of unique observed days

- Coverage: proportion of observed days relative to the total number of
  calendar days within the observed range

- Fundamental quantiles (5\\

- Interquartile range (IQR) in days

Weekday and month distributions are compared to their expected
probabilities using chi-square goodness-of-fit tests.

Standardized residuals are defined as \$\$(Observed - Expected) /
sqrt(Expected)\$\$. They describe the magnitude and direction of
deviation from the expected distribution.

Sentinel detection is based on simple heuristics such as extremely large
future dates or implausibly early calendar dates. It is meant as a
diagnostic aid rather than a formal validation procedure.

## See also

[`desc`](Desc.md), [`print.Desc.Date`](print.Desc.Date.md)

Other desc: [`desc()`](Desc.md), [`desc.factor()`](Desc.factor.md),
[`desc.nn`](Desc.nn.md), [`desc.nq`](desc.nq.md),
[`desc.numeric()`](desc.numeric.md), [`desc.qn`](desc.qn.md),
[`desc.qq`](desc.qq.md), [`desc.ts()`](desc.ts.md),
[`print.Desc.qq()`](desc.table.md)
