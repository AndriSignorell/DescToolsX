# Bland-Altman Agreement Data

Computes the quantities required for a Bland-Altman agreement analysis.

## Usage

``` r
blandAltmanData(x, ...)

# Default S3 method
blandAltmanData(x, y, conf.level = 0.95, na.rm = FALSE, ...)

# S3 method for class 'formula'
blandAltmanData(x, data = NULL, conf.level = 0.95, na.rm = FALSE, ...)
```

## Arguments

- x:

  numeric vector or formula

- ...:

  further arguments passed to or from other methods

- y:

  numeric vector

- conf.level:

  confidence level for the intervals reported for the bias and the
  limits of agreement

- na.rm:

  logical; if `TRUE`, incomplete observation pairs are removed before
  computation

- data:

  optional data frame used with the formula interface

## Value

an object of class `"BlandAltman"` with components:

- `mean`:

  pairwise means

- `diff`:

  pairwise differences (`y - x`)

- `bias`:

  mean difference

- `loaLower`:

  lower limit of agreement

- `loaUpper`:

  upper limit of agreement

- `biasCI`:

  confidence interval for the bias

- `loaLowerCI`:

  confidence interval for the lower LoA

- `loaUpperCI`:

  confidence interval for the upper LoA

- `nObs`:

  number of observations used

- `conf.level`:

  the confidence level used

## Details

For each observation pair, the arithmetic mean and the difference
(`y - x`) are calculated. The function further computes the mean
difference (bias), limits of agreement (LoA), and approximate confidence
intervals for the bias and LoA according to Bland and Altman.

The returned object is of class `"BlandAltman"` and can be plotted using
by a [`plot()`](https://rdrr.io/r/graphics/plot.default.html) routine
residing in the pharos package.

The limits of agreement are the conventional `bias +/- 1.96 * sd`,
independent of `conf.level`: the multiplier fixes the nominal coverage
of the interval of *differences*, whereas `conf.level` governs the
confidence intervals reported for the bias and for the two limits. The
standard error of a limit uses the approximation \\\sqrt{3 s_d^2 / n}\\
given by Bland and Altman.

## References

Bland JM, Altman DG (1986). Statistical methods for assessing agreement
between two methods of clinical measurement. *Lancet*, 327, 307-310.

## See also

Other agreement: [`normalizeToConfusion()`](normalizeToConfusion.md),
[`raterFrame()`](raterFrame.md)

## Examples

``` r
set.seed(1)
x <- rnorm(100)
y <- x + rnorm(100, sd = 0.5)

ba <- blandAltmanData(x, y)
ba
#> 
#> Bland-Altman Agreement Analysis
#> 
#> Bias      : -0.019 (-0.114, 0.076)
#> Lower LoA : -0.958 (-1.122, -0.793)
#> Upper LoA : 0.920 (0.755, 1.084)
#> 
#> n = 100, conf.level = 0.95
```
