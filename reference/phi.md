# Phi Coefficient

Computes the Phi coefficient as a measure of association between two
categorical variables.

## Usage

``` r
phi(x, y = NULL, ...)
```

## Arguments

- x:

  a vector of categorical data or a contingency table (matrix or
  `table`)

- y:

  optional second categorical vector. If provided, a contingency table
  is constructed from `x` and `y`.

- ...:

  additional arguments passed to
  [`table`](https://rdrr.io/r/base/table.html)

## Value

a numeric scalar containing the Phi coefficient

## Details

If `y` is provided, a contingency table is created using
`table(x, y, ...)`. Otherwise, `x` is assumed to already be a
contingency table.

Note: Yates' continuity correction is **not applied** when computing the
chi-squared statistic.

The Phi coefficient is defined as: \$\$ \phi = \sqrt{ \frac{\chi^2}{n} }
\$\$ where \\\chi^2\\ is the chi-squared test statistic and \\n\\ is the
total sample size.

This definition is **unsigned**. For a 2x2 table the signed coefficient
\\(n\_{11} n\_{22} - n\_{12} n\_{21}) / \sqrt{n\_{1\cdot} n\_{2\cdot}
n\_{\cdot 1} n\_{\cdot 2}}\\ equals the Pearson correlation of the two
0/1 indicators and lies in \\\[-1, 1\]\\; the value returned here is its
absolute value, so the direction of the association is not reported. See
[`pearsonCor`](pearsonCor.md) if the sign is needed.

For contingency tables larger than 2x2, Phi is not bounded by 1 and may
exceed 1. In such cases, [`cramerV`](cramerV.md) is usually preferred.

## See also

[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html)

Other assoc.nominal: [`contCoef()`](contCoef.md),
[`cramerV()`](cramerV.md), [`gkTau()`](gkTau.md),
[`lambda()`](lambda.md), [`mutInf()`](mutInf.md),
[`tschuprowT()`](tschuprowT.md), [`uncertCoef()`](uncertCoef.md),
[`yule`](yule.md)

## Examples

``` r
# Example with vectors
x <- c("A", "A", "B", "B")
y <- c("yes", "no", "yes", "no")
phi(x, y)
#> [1] 0

# Example with contingency table. Note that the signed coefficient is
# -0.0891 here: phi() reports the magnitude only.
tab <- matrix(c(10, 20, 30, 40), nrow = 2)
phi(tab)
#> [1] 0.08908708

```
