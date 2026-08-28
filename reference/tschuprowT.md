# Tschuprow's T

Computes Tschuprow's T, a measure of association between two categorical
variables based on the chi-squared statistic.

## Usage

``` r
tschuprowT(x, y = NULL, correct = FALSE, ...)
```

## Arguments

- x:

  a vector of categorical data (then `y` must be given) or a
  two-dimensional contingency table (matrix or `table`)

- y:

  optional second categorical vector. If provided, a contingency table
  is constructed from `x` and `y`.

- correct:

  logical; if `TRUE`, applies a bias correction according to Bergsma
  (2013).

- ...:

  additional arguments passed to
  [`table`](https://rdrr.io/r/base/table.html). This refers only to the
  vector interface.

## Value

a numeric scalar containing Tschuprow's T

## Details

If `y` is provided, a contingency table is created using
`table(x, y, ...)`. Otherwise, `x` is assumed to already be a
two-dimensional contingency table.

Tschuprow's T is defined as: \$\$ T = \sqrt{ \frac{\chi^2}{n \sqrt{(r -
1)(c - 1)}} } \$\$ where \\\chi^2\\ is the chi-squared statistic, \\n\\
is the total sample size, and \\r\\ and \\c\\ are the number of rows and
columns of the contingency table.

If `correct = TRUE`, a bias-corrected version is computed based on
Bergsma (2013), which adjusts the estimate especially for small samples.
It replaces \\\phi^2 = \chi^2/n\\ by \\\tilde\phi^2 = \max(0, \phi^2 -
(r-1)(c-1)/(n-1))\\ and the dimensions by \\\tilde r = r -
(r-1)^2/(n-1)\\ and \\\tilde c = c - (c-1)^2/(n-1)\\.

For a 2x2 table T coincides with Cramer's V and with the absolute value
of the phi coefficient; the sign of the association is not reported.

## References

Tschuprow, A. A. (1939). *Principles of the Mathematical Theory of
Correlation*. W. Hodge & Co.

Bergsma, W. (2013). A bias-correction for Cramer's V and Tschuprow's T.
*Journal of the Korean Statistical Society*, 42(3), 323–328.
https://doi.org/10.1016/j.jkss.2012.10.002

## See also

[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html),
[`cramerV`](cramerV.md)

Other assoc.nominal: [`contCoef()`](contCoef.md),
[`cramerV()`](cramerV.md), [`gkTau()`](gkTau.md),
[`lambda()`](lambda.md), [`mutInf()`](mutInf.md), [`phi()`](phi.md),
[`uncertCoef()`](uncertCoef.md), [`yule`](yule.md)

## Examples

``` r
# Example with vectors
x <- c("A", "A", "B", "B")
y <- c("yes", "no", "yes", "no")
tschuprowT(x, y)
#> [1] 0

# Example with contingency table
tab <- matrix(c(10, 20, 30, 40), nrow = 2)
tschuprowT(tab)               # 0.08908708
#> [1] 0.08908708

# Bias-corrected version: the correction exceeds the estimate here,
# so the corrected value is 0
tschuprowT(tab, correct = TRUE)
#> [1] 0
```
