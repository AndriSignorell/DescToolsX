# K-Nearest Neighbors Imputation

Imputes missing values from the `k` nearest complete observations.
Numeric and factor variables are both supported.

## Usage

``` r
imputeKnn(
  x,
  k = 10,
  scale = TRUE,
  method = c("weighted", "median"),
  distData = NULL
)
```

## Arguments

- x:

  a data frame with missing values.

- k:

  positive integer, the number of nearest neighbours to draw on.

- scale:

  logical; if `TRUE` (default), numeric variables are standardized
  before distances are computed. This affects the distances only, not
  the imputed values.

- method:

  the aggregation applied to the neighbours' values, either `"weighted"`
  for a distance-weighted mean (numeric) or weighted mode (factor), or
  `"median"` for a median (numeric) or mode (factor).

- distData:

  optional data frame supplying the pool of potential neighbours. If
  given, neighbours are drawn from `distData` only, while `x` alone is
  imputed. It must have the same variables as `x`.

## Value

a data frame of the same shape as `x`, with missing values replaced.

## Details

Distance is Gower-style: numeric variables contribute their squared
differences and factor variables a 0/1 mismatch penalty, summed and
square-rooted. Numeric variables are standardized beforehand when
`scale = TRUE`, so that no variable dominates through its unit alone.
The standardization affects distances only; imputed values are always
taken from the original data.

Continuous and categorical contributions are combined without further
weighting, so a factor mismatch counts as much as a
one-standard-deviation numeric gap. Whether that balance suits the data
is for the caller to judge.

Each incomplete observation is compared with the complete ones on the
variables it does observe, so rows missing different variables use
different distances. Observations are therefore grouped by their pattern
of missingness and each pattern is solved in one pass.

With `method = "weighted"` the neighbours are weighted by \\\exp(-d)\\.
Because that decays on the scale of the distances themselves, it is
meaningful only when those are of moderate size, which is another reason
to keep `scale = TRUE`. Weights are normalized against their maximum
before exponentiation, so that widely separated neighbours cannot all
underflow to zero.

When dbscan is installed it is used to find the neighbours via a
kd-tree, which is markedly faster on large data. The result is the same
either way, apart from the resolution of exact ties.

## See also

Other impute: [`impute()`](impute.md)

## Examples

``` r
set.seed(123)
dat <- data.frame(
  x = c(1, 2, 3, 4, 5, 6),
  y = c(1, 2, 3, 4, 5, 6),
  z = factor(c("a", "b", "a", "b", "a", "b"))
)

dat[c(1, 3), "x"] <- NA
dat[c(2, 5), "y"] <- NA

imputeKnn(dat, k = 2)
#>          x       y z
#> 1 4.599849 1.00000 a
#> 2 2.000000 4.47332 b
#> 3 4.688363 3.00000 a
#> 4 4.000000 4.00000 b
#> 5 5.000000 5.00000 a
#> 6 6.000000 6.00000 b

# neighbours drawn from a separate reference set
ref <- data.frame(
  x = c(1.5, 2.5, 3.5, 4.5),
  y = c(1.5, 2.5, 3.5, 4.5),
  z = factor(c("a", "b", "a", "b"), levels = c("a", "b"))
)

imputeKnn(dat, k = 2, distData = ref)
#>          x        y z
#> 1 1.758634 1.000000 a
#> 2 2.000000 2.174485 b
#> 3 2.798218 3.000000 a
#> 4 4.000000 4.000000 b
#> 5 5.000000 3.978892 a
#> 6 6.000000 6.000000 b
```
