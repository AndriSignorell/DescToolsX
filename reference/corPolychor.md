# Polychoric Correlation

Estimates the polychoric correlation between two ordinal variables based
on a contingency table. Both a two-step estimator and full maximum
likelihood (ml) estimation are supported.

## Usage

``` r
corPolychor(
  x,
  y = NULL,
  method = c("two-step", "ml"),
  se = FALSE,
  control = list(),
  maxcor = 0.9999,
  ...
)
```

## Arguments

- x:

  a contingency table or an ordinal vector

- y:

  optional second ordinal vector. If supplied, a contingency table is
  constructed via `table(x, y, ...)`.

- method:

  character string specifying the estimation method:

  `"two-step"`

  :   two-step estimator (default, fast)

  `"ml"`

  :   full maximum likelihood estimation

- se:

  logical; if `TRUE`, standard errors are computed via the Hessian
  matrix. This requires ml estimation, so it is an error to combine it
  with `method = "two-step"`.

- control:

  a list of control parameters passed to
  [`optim`](https://rdrr.io/r/stats/optim.html)

- maxcor:

  numeric; maximum absolute correlation allowed (default `0.9999`) to
  avoid numerical issues near the boundary

- ...:

  further arguments passed to
  [`table`](https://rdrr.io/r/base/table.html) when `y` is supplied, for
  example `useNA`

## Value

if `se = FALSE`, a numeric value giving the estimated correlation.

If `se = TRUE`, a list with components:

- `type`:

  type of correlation

- `rho`:

  estimated polychoric correlation

- `rowCuts`:

  estimated row thresholds

- `colCuts`:

  estimated column thresholds

- `var`:

  variance-covariance matrix of the estimates, on the scale of `rho` and
  the thresholds. The optimiser works on `atanh(rho)`, so the leading
  row and column are transformed back with the delta method, factor
  \\(1 - \rho^2)\\.

- `n`:

  total sample size

- `chisq`:

  likelihood-ratio test statistic

- `df`:

  degrees of freedom

- `method`:

  estimation method actually used

The returned object has class `"Polychor"`.

## Details

The polychoric correlation estimates the correlation between two latent
normally distributed variables underlying observed ordinal variables.

The likelihood is based on a discretized bivariate normal distribution,
evaluated via [`pmvnorm`](https://rdrr.io/pkg/mvtnorm/man/pmvnorm.html).

For numerical stability:

- The correlation parameter is internally transformed using
  [`tanh()`](https://rdrr.io/r/base/Hyperbolic.html) to enforce
  \\\|\rho\| \< 1\\. The search range on that scale is derived from
  `maxcor`, so the estimate is free to approach the documented boundary.

- Cell probabilities are bounded away from zero to avoid `log(0)`.

Empty rows or columns in the contingency table are removed with a
warning.

## References

Olsson, U. (1979). Maximum likelihood estimation of the polychoric
correlation coefficient. *Psychometrika*, 44(4), 443–460.

Fox, J. (2016). *Applied Regression Analysis and Generalized Linear
Models*.

## See also

[`pmvnorm`](https://rdrr.io/pkg/mvtnorm/man/pmvnorm.html),
[`optim`](https://rdrr.io/r/stats/optim.html)

Other assoc.continuous: [`corPart()`](corPart.md),
[`findCorrX()`](findCorrX.md), [`hoeffdingD()`](hoeffdingD.md),
[`keepSig()`](keepSig.md), [`pearsonCor()`](pearsonCor.md),
[`spearmanCor()`](spearmanCor.md)

## Examples

``` r
# Example with ordinal variables
set.seed(1)
z <- rnorm(200)
x <- factor(cut(z + rnorm(200, sd = 0.6), 3), ordered = TRUE)
y <- factor(cut(z + rnorm(200, sd = 0.6), 3), ordered = TRUE)

# Two-step estimate
corPolychor(x, y)
#> [1] 0.6300061

# ml estimate
corPolychor(x, y, method = "ml")
#> [1] 0.6290183

# With standard errors
res <- corPolychor(x, y, method = "ml", se = TRUE)
res$rho
#> [1] 0.6290183
```
