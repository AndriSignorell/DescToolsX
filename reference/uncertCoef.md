# Uncertainty Coefficient

Computes directional or symmetric uncertainty coefficients. The
directional coefficient U(C\|R) measures the proportion of uncertainty
(entropy) in the column variable Y that is explained by the row variable
X. The function has interfaces for a table, a matrix, a data frame, and
single vectors.

## Usage

``` r
uncertCoef(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  direction = c("symmetric", "row", "column"),
  pZeroCorrection = 1/sum(x)^2,
  ...
)
```

## Arguments

- x:

  a numeric vector, factor, matrix, or data frame

- y:

  `NULL` (default) or a vector, an ordered factor, matrix or data frame
  with compatible dimensions to `x`

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- direction:

  direction of calculation, one of `"symmetric"` (default), `"row"`, or
  `"column"`. The row direction calculates U(R\|C), and the column
  direction calculates U(C\|R).

- pZeroCorrection:

  small positive value used to replace zero cells before taking
  logarithms

- ...:

  further arguments are passed to the function
  [`table`](https://rdrr.io/r/base/table.html), allowing, for example,
  `useNA` to be set. This refers only to the vector interface.

## Value

if `conf.level = NA`, a numeric scalar. Otherwise a named numeric vector
with elements:

- `est`:

  uncertainty coefficient estimate

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

The uncertainty coefficient is computed as \$\$U(C\|R) = \frac{H(X) +
H(Y) - H(XY)}{H(Y)} \$\$ and ranges from `[0, 1]`.

## Note

Based on code from Antti Arppe

## Confidence interval

The interval is based on the asymptotic standard error (Goodman &
Kruskal) and is truncated to the parameter range \\\[0, 1\]\\. For a
one-sided interval (`sides = "left"` or `"right"`) the open side is
reported at the corresponding range limit, not at \\\pm\infty\\.

## References

Theil, H. (1972), *Statistical Decomposition Analysis*, Amsterdam:
North-Holland Publishing Company.

## See also

[`Association`](Association.md)

Other assoc.nominal: [`contCoef()`](contCoef.md),
[`cramerV()`](cramerV.md), [`gkTau()`](gkTau.md),
[`lambda()`](lambda.md), [`mutInf()`](mutInf.md), [`phi()`](phi.md),
[`tschuprowT()`](tschuprowT.md), [`yule`](yule.md)

## Examples

``` r

# example from Goodman Kruskal (1954)

m <- as.table(cbind(c(1768,946,115), c(807,1387,438), c(189,746,288), c(47,53,16)))
dimnames(m) <- list(paste("A", 1:3), paste("B", 1:4))
m
#>      B 1  B 2  B 3  B 4
#> A 1 1768  807  189   47
#> A 2  946 1387  746   53
#> A 3  115  438  288   16

# direction default is "symmetric"
uncertCoef(m)
#> [1] 0.07991026
uncertCoef(m, conf.level=0.95)
#>        est        lci        uci 
#> 0.07991026 0.07131377 0.08850675 

uncertCoef(m, direction="row")
#> [1] 0.08506956
uncertCoef(m, direction="column")
#> [1] 0.07534098
```
