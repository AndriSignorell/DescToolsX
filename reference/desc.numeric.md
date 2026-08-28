# Describe a Numeric Variable

Compute descriptive statistics for a numeric vector. The plot method may
display a histogram, density curve, box plot, and empirical
distribution.

## Usage

``` r
# S3 method for class 'numeric'
desc(
  x,
  maxrows = NULL,
  conf.level = 0.95,
  include_x = TRUE,
  main = NULL,
  verbose = NULL,
  plotit = NULL,
  digits = NULL,
  ...
)
```

## Arguments

- x:

  numeric vector to describe, or an object of class `"Desc.numeric"` for
  the print and plot methods

- maxrows:

  numeric; defines the maximum number of rows in a frequency table to be
  reported. For factors with many levels it is often not interesting to
  see all of them. Default is set to 12 most frequent ones (resp. the
  first ones if `ord` is set to `"levels"` or `"names"`).

- conf.level:

  confidence level for interval estimates (default 0.95)

- include_x:

  logical; if `TRUE`, the original vector is retained in the result

- main:

  character string, `NULL`, or `NA`, defining the main title. By default
  (`main = NULL`) the title will be composed as: (\<class(es)\>). If
  `NA`, no title is printed.

- verbose:

  integer controlling verbosity of table output. One of `1` (minimal),
  `2` (default), `3` (extensive). Applies to tables only.

- plotit:

  logical. Should a plot be created? The plot type depends on the
  classes of the variables. Default can be defined by the option
  `plotit`, if it does not exist then it's set to `TRUE`.

- digits:

  number of digits used to format numeric values

- ...:

  further arguments passed to methods

## Value

an object of class `c("Desc.numeric", "Desc")` containing descriptive
statistics, frequency information, and metadata

## Details

This function is an S3 method for
[`plot`](https://rdrr.io/r/graphics/plot.default.html). It is
automatically dispatched when calling `plot(x)` on a `Desc.numeric`
object.

For a numeric argument x `maxrows` is the minimum number of unique
values needed for a numeric variable to be treated as continuous. If
left to its default `NULL`, x will be regarded as continuous if it has
more than 12 single values. In this case the list of extreme values will
be displayed and the frequency table else.

If `maxrows` is \< 1 it will be interpreted as percentage. In this case
just as many rows, as the `maxrows` most frequent levels will be shown.
Say, if `maxrows` is set to `0.8`, then the number of rows is fixed so,
that the highest cumulative relative frequency is the first one going
beyond 0.8.

Setting `maxrows` to `Inf` will unconditionally report all values and
also produce a plot with type "h" instead of a histogram.

Named colors defined by `DescToolsX` (e.g. `"hred"`, `"hblue"`) can be
used directly.

## See also

[`base::summary()`](https://rdrr.io/r/base/summary.html),
[`base::plot()`](https://rdrr.io/r/base/plot.html)

Other Statistical summary functions: [`abstract()`](abstract.md)

Other desc: [`desc()`](Desc.md), [`desc.Date()`](Desc.Date.md),
[`desc.factor()`](Desc.factor.md), [`desc.nn`](Desc.nn.md),
[`desc.nq`](desc.nq.md), [`desc.qn`](desc.qn.md),
[`desc.qq`](desc.qq.md), [`desc.ts()`](desc.ts.md),
[`print.Desc.qq()`](desc.table.md)

## Examples

``` r

desc(Pizza$delivery_min)             # numeric
#> ────────────────────────────────────────────────────────────────────────────── 
#> Pizza$delivery_min (numeric) :
#>   Delivery time in minutes.
#> 
#> 
#> length       n    NAs  unique     0s   mean  meanCI¹
#>   1209    1209      0     384      0  25.65   25.04
#>         100.0%   0.0%           0.0%          26.26
#>                                                    
#>    .05     .10    .25  median    .75    .90     .95
#>  10.40   11.60  17.40   24.40  32.50  40.42   45.20
#>                                                    
#>  range      sd  vcoef     mad    iqr   skew    kurt
#>  56.80   10.84   0.42   11.27  15.10   0.61    0.10
#>                                                    
#> lowest : 8.8 (3), 8.9, 9.0 (3), 9.1 (5), 9.2 (3)
#> highest: 61.9, 62.7, 62.9, 63.2, 65.6
#> 
#> ¹ 95%-CI (classic)
#> 

```
