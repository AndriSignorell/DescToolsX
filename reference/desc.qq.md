# Describe Relationship: Categorical x by Categorical y

Computes, prints and plots descriptive statistics for the relationship
between two categorical variables `x` and `y`. The function is
dispatched automatically by `desc(y ~ x, data)` when both variables are
categorical.

## Usage

``` r
.descQQ(x, y, ...)

# S3 method for class 'Desc.qq'
print(x, digits = NULL, ...)

# S3 method for class 'Desc.qq'
plot(x, main = x$meta$main, which = 1, ...)
```

## Arguments

- x:

  a categorical variable for `.descQQ()`, or an object of class
  `"Desc.qq"` for the print and plot methods

- digits:

  currently unused

- ...:

  further arguments. In `.descQQ()` passed to [`desc()`](Desc.md), in
  [`print()`](https://rdrr.io/r/base/print.html) to
  [`print.Desc.table()`](desc.table.md), in
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) to
  [`pharos::plot.Desc.table()`](https://andrisignorell.github.io/pharos/reference/plot.Desc.table.html).

- main:

  main title for the plot; defaults to the title stored in `x$meta$main`

- which:

  integer vector selecting the plots to draw, passed on to
  [`pharos::plot.Desc.table()`](https://andrisignorell.github.io/pharos/reference/plot.Desc.table.html),
  see section **Plots**. Default `1`.

- y:

  a categorical variable

## Value

`.descQQ()` returns an object of class `c("Desc.qq", "Desc")`. The plot
method returns the value of
[`pharos::plot.Desc.table()`](https://andrisignorell.github.io/pharos/reference/plot.Desc.table.html).

## Details

This function is a wrapper around [`desc.table()`](desc.table.md)
applied to the contingency table `table(x, y)`.

It summarizes the joint distribution of two categorical variables and
provides association measures and visualizations.

**Computed statistics**

- Contingency table

- Row and column percentages

- Association measures (e.g., Cramer's V, Phi)

- Optional statistical tests depending on configuration

**Implementation note** Internally, `desc.qq(x, y)` is equivalent to:


    desc(table(x, y))

## Plots

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) labels the
table dimensions with the variable names and hands over to
[`pharos::plot.Desc.table()`](https://andrisignorell.github.io/pharos/reference/plot.Desc.table.html),
the plot method for contingency tables. Its help page documents the
displays selected by `which` and all further arguments; the displays
themselves are drawn by
[`pharos::plotMosaic()`](https://andrisignorell.github.io/pharos/reference/plotMosaic.html),
[`pharos::plotAssoc()`](https://andrisignorell.github.io/pharos/reference/plotAssoc.html)
and
[`pharos::plotHeatmap()`](https://andrisignorell.github.io/pharos/reference/plotHeatmap.html),
among others.

`main` defaults to the title stored in the object.

## See also

[`desc()`](Desc.md), [`desc.table()`](desc.table.md),
[`desc.qn()`](desc.qn.md), [`desc.nq()`](desc.nq.md),
[`desc.nn()`](Desc.nn.md)

Plot method:
[`pharos::plot.Desc.table()`](https://andrisignorell.github.io/pharos/reference/plot.Desc.table.html)

Other desc: [`desc()`](Desc.md), [`desc.Date()`](Desc.Date.md),
[`desc.factor()`](Desc.factor.md), [`desc.nn`](Desc.nn.md),
[`desc.nq`](desc.nq.md), [`desc.numeric()`](desc.numeric.md),
[`desc.qn`](desc.qn.md), [`desc.table()`](desc.table.md),
[`desc.ts()`](desc.ts.md)

## Examples

``` r
# basic usage via desc()
desc(quality ~ area, Pizza)
#> ────────────────────────────────────────────────────────────────────────────── 
#> quality ~ area (Pizza) (Desc.qq)
#> 
#> Summary:
#> pairs: 1209, valid: 999 (82.6%), missings: 210 (17.4%)
#> 
#>                  Brent   Camden Westminster      Sum
#>                                                     
#> low    freq         30       46          79      155
#>        p.row     19.4%    29.7%       51.0%    15.5%
#> 
#> medium freq        134       97         122      353
#>        p.row     38.0%    27.5%       34.6%    35.3%
#> 
#> high   freq        232      144         115      491
#>        p.row     47.3%    29.3%       23.4%    49.1%
#> 
#> Sum    freq        396      287         316      999
#>        p.row     39.6%    28.7%       31.6%   100.0% 
#> 
#> 
#> Pearson's Chi-squared test:
#>   X-squared = 53.559, df = 4, p-value = 6.509e-11
#> Log likelihood ratio (G-test) test of independence:
#>   G = 55.05, df = 4, p-value = 3.171e-11
#> Mantel-Haenszel Chi-squared:
#>   X-squared = 51.341, df = 1, p-value = 7.762e-13
#> 
#> Contingency Coeff.   0.226
#> Cramer V             0.164
#> Kendall Tau-b       -0.196
#> 


# store result, print and plot separately
d <- desc(quality ~ area, Pizza, plotit = FALSE)
d
#> ────────────────────────────────────────────────────────────────────────────── 
#> quality ~ area (Pizza) (Desc.qq)
#> 
#> Summary:
#> pairs: 1209, valid: 999 (82.6%), missings: 210 (17.4%)
#> 
#>                  Brent   Camden Westminster      Sum
#>                                                     
#> low    freq         30       46          79      155
#>        p.row     19.4%    29.7%       51.0%    15.5%
#> 
#> medium freq        134       97         122      353
#>        p.row     38.0%    27.5%       34.6%    35.3%
#> 
#> high   freq        232      144         115      491
#>        p.row     47.3%    29.3%       23.4%    49.1%
#> 
#> Sum    freq        396      287         316      999
#>        p.row     39.6%    28.7%       31.6%   100.0% 
#> 
#> 
#> Pearson's Chi-squared test:
#>   X-squared = 53.559, df = 4, p-value = 6.509e-11
#> Log likelihood ratio (G-test) test of independence:
#>   G = 55.05, df = 4, p-value = 3.171e-11
#> Mantel-Haenszel Chi-squared:
#>   X-squared = 51.341, df = 1, p-value = 7.762e-13
#> 
#> Contingency Coeff.   0.226
#> Cramer V             0.164
#> Kendall Tau-b       -0.196
#> 

# the plots, see pharos::plot.Desc.table()
plot(d, which = 1)

plot(d, which = 2)

plot(d, which = 3)

plot(d, which = 4)                     # association plot

plot(d, which = 5)                     # heatmap


# pipe
desc(quality ~ area, Pizza) |> plot(which = 4)

```
