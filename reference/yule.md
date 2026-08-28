# Yule's Coefficients of Association (Q and Y)

Computes Yule's Q or Y for a 2x2 contingency table, optionally with
asymptotic confidence interval based on the log odds ratio.

## Usage

``` r
yuleQ(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  correct = FALSE,
  ...
)

yuleY(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  correct = FALSE,
  ...
)
```

## Arguments

- x:

  a 2x2 contingency table (matrix or table). If `y` is supplied, `x` and
  `y` are cross-tabulated via
  [`table()`](https://rdrr.io/r/base/table.html).

- y:

  optional second variable for cross-tabulation

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See details
  in [`ConfidenceIntervals`](ConfidenceIntervals.md).

- correct:

  logical; if `TRUE`, applies the Haldane–Anscombe correction by adding
  0.5 to all cells

- ...:

  further arguments passed to
  [`table()`](https://rdrr.io/r/base/table.html)

## Value

if `conf.level = NA`, a numeric scalar. Otherwise a named numeric vector
with elements:

- `est`:

  point estimate of Yule's Q or Y

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

For a 2x2 table with cell counts \\a, b, c, d\\:

Odds ratio: \$\$OR = \frac{ad}{bc}\$\$

Yule's Q: \$\$Q = \frac{OR - 1}{OR + 1} =
\tanh\left(\frac{1}{2}\log(OR)\right)\$\$

Yule's Y: \$\$Y = \frac{\sqrt{OR} - 1}{\sqrt{OR} + 1} =
\tanh\left(\frac{1}{4}\log(OR)\right)\$\$

Both coefficients are computed from the `tanh` form, which stays well
defined when a zero cell drives the odds ratio to 0 or `Inf` (the
coefficient is then -1 or 1).

Confidence intervals are obtained from the asymptotic normal
approximation: \$\$\log(OR) \pm z \cdot \sqrt{1/a + 1/b + 1/c + 1/d}\$\$
and then transformed to the selected coefficient. With a zero cell the
standard error is infinite and the interval degenerates to \\\[-1,
1\]\\; use `correct = TRUE` to obtain a finite interval.

For a one-sided interval the open side is reported at the range limit
(-1 resp. 1), not at \\\pm\infty\\.

## References

Yule, G. U. (1912). On the methods of measuring association between two
attributes.

## See also

Other assoc.nominal: [`contCoef()`](contCoef.md),
[`cramerV()`](cramerV.md), [`gkTau()`](gkTau.md),
[`lambda()`](lambda.md), [`mutInf()`](mutInf.md), [`phi()`](phi.md),
[`tschuprowT()`](tschuprowT.md), [`uncertCoef()`](uncertCoef.md)

## Examples

``` r
m <- matrix(c(12, 5, 3, 20), nrow = 2)
yuleQ(m)                        # 0.8823529
#> [1] 0.8823529
yuleY(m, conf.level = 0.95)     # 0.6
#>       est       lci       uci 
#> 0.6000000 0.2849591 0.7980515 

# a zero cell yields the limiting value 1 (and not NaN)
yuleQ(matrix(c(12, 5, 0, 20), nrow = 2), conf.level = NA)
#> [1] 1

# ... a finite interval requires the Haldane-Anscombe correction
yuleQ(matrix(c(12, 5, 0, 20), nrow = 2), correct = TRUE)
#> [1] 0.9787645
```
