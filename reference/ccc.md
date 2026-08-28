# Lin's Concordance Correlation Coefficient

Computes Lin's concordance correlation coefficient (CCC) for assessing
agreement between two continuous measurements.

## Usage

``` r
ccc(
  x,
  y,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  method = c("z-transform", "boot", "asymptotic"),
  na.rm = FALSE,
  ...
)
```

## Arguments

- x:

  a numeric vector

- y:

  a numeric vector of equal length to `x`

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- method:

  a character string specifying the confidence interval method. One of
  `"z-transform"`, `"boot"`, or `"asymptotic"`.

- na.rm:

  logical; if `TRUE`, incomplete observation pairs are removed before
  computation

- ...:

  additional arguments controlling the bootstrap procedure. Currently
  `R`, `parallel` and `ncpus` are supported.

## Value

a named numeric vector containing only `est` when `conf.level = NA`;
otherwise a named numeric vector with elements:

- `est`:

  point estimate.

- `lci`:

  lower confidence interval bound.

- `uci`:

  upper confidence interval bound.

Additional diagnostics are stored as attributes:

- `nObs`:

  number of observations used

- `scaleShift`:

  scale shift parameter

- `locationShift`:

  location shift parameter

- `biasCorrection`:

  bias correction factor

- `method`:

  confidence interval method, if applicable

- `confLevel`:

  confidence level, if applicable

- `sides`:

  confidence interval type, if applicable

## Details

The CCC combines measures of precision and accuracy and quantifies the
deviation of the observed data from the line of perfect concordance.
Values range from -1 to 1, where 1 indicates perfect agreement.

Confidence intervals can be computed using a Fisher z-transformation, a
nonparametric bootstrap, or the asymptotic approximation of Lin (2000).
The asymptotic variance implemented here is the corrected form given by
Lin (2000), superseding the expression in Lin (1989). Internally it is
held on the scale of \\\rho_c\\ itself; the `"z-transform"` method
rescales it to the z scale via the delta method, where
\\d\\\mathrm{atanh}(\rho)/d\rho = 1/(1 - \rho^2)\\.

`sides` names the side on which the finite bound lies: `"left"` yields
an interval bounded below, with the upper limit fixed at 1, and
`"right"` one bounded above, with the lower limit fixed at -1.

Missing values are handled according to package conventions: if
`na.rm = FALSE` and either `x` or `y` contains missing values, `NA` is
returned. If `na.rm = TRUE`, complete cases are used. Infinite values
carry no comparable convention - they leave the moments undefined and
are rejected with an error.

## References

Lin, L. I.-K. (1989). A concordance correlation coefficient to evaluate
reproducibility. *Biometrics*, *45*(1), 255-268.

Lin, L. I.-K. (2000). A note on the concordance correlation coefficient.
*Biometrics*, *56*(1), 324-325.

## See also

Other assoc.agreement: [`cohenKappa()`](cohenKappa.md),
[`cronbachAlpha()`](cronbachAlpha.md), [`icc()`](icc.md),
[`kappaM()`](kappaM.md), [`krippAlpha()`](krippAlpha.md),
[`pabak()`](pabak.md), [`percAgreement()`](percAgreement.md),
[`randolphKappa()`](randolphKappa.md)

## Examples

``` r
set.seed(123)

x <- rnorm(100)
y <- x + rnorm(100, sd = 0.2)

ccc(x, y)
#>       est 
#> 0.9775499 
#> attr(,"nObs")
#> [1] 100
#> attr(,"scaleShift")
#> [1] 1.011879
#> attr(,"locationShift")
#> [1] -0.02354303
#> attr(,"biasCorrection")
#> [1] 0.9996533

ccc(x, y, conf.level = 0.95)
#>       est       lci       uci 
#> 0.9775499 0.9668474 0.9848240 
#> attr(,"nObs")
#> [1] 100
#> attr(,"scaleShift")
#> [1] 1.011879
#> attr(,"locationShift")
#> [1] -0.02354303
#> attr(,"biasCorrection")
#> [1] 0.9996533
#> attr(,"method")
#> [1] "z-transform"
#> attr(,"confLevel")
#> [1] 0.95
#> attr(,"sides")
#> [1] "two.sided"

ccc(
  x, y,
  conf.level = 0.95,
  method = "boot",
  R = 999
)
#>       est       lci       uci 
#> 0.9775499 0.9666290 0.9851318 
#> attr(,"nObs")
#> [1] 100
#> attr(,"scaleShift")
#> [1] 1.011879
#> attr(,"locationShift")
#> [1] -0.02354303
#> attr(,"biasCorrection")
#> [1] 0.9996533
#> attr(,"method")
#> [1] "boot"
#> attr(,"confLevel")
#> [1] 0.95
#> attr(,"sides")
#> [1] "two.sided"
```
