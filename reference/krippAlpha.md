# Krippendorff's Alpha for Wide Data

Computes Krippendorff's alpha coefficient of interrater reliability from
data in wide format (with \\m\\ raters). The function supports nominal,
ordinal, interval, and ratio measurement levels.

## Usage

``` r
krippAlpha(
  x,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  metric = c("nominal", "ordinal", "interval", "ratio"),
  levels = NULL,
  raters = NULL,
  output = c("def", "ext"),
  ...
)
```

## Arguments

- x:

  a data frame, matrix, or similar wide-format object containing ratings
  (columns = raters, rows = subjects/items)

- conf.level:

  confidence level for bootstrap confidence intervals of Krippendorff's
  alpha. If `NA` (default), no bootstrap is computed.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See details
  in [`ConfidenceIntervals`](ConfidenceIntervals.md). Alpha lies in
  \\\[-1, 1\]\\, so the open side is reported at that boundary rather
  than at an infinity it cannot reach.

- metric:

  character string specifying the measurement level, i.e. the difference
  function \\\delta^2\\ used to compare two categories. One of
  `"nominal"`, `"ordinal"`, `"interval"`, or `"ratio"`. This selects
  *which* alpha is computed and has nothing to do with the confidence
  interval - the bootstrap interval type travels as `type` through
  `...`. It is called `metric` and not `method` because `method` means
  the interval method everywhere else in the suite.

- levels:

  optional vector specifying the possible categories or scale values
  (required for the `"interval"` and `"ratio"` metrics). If `NULL`,
  levels are inferred from the data.

- raters:

  optional vector specifying which columns of `x` are the raters. If
  `NULL`, all columns are assumed to be raters.

- output:

  output format, either `"def"` (default) or `"ext"` for extended
  results

- ...:

  further arguments passed to
  [`boot`](https://rdrr.io/pkg/boot/man/boot.html). Supported arguments
  are `type` (`"norm"`, `"basic"`, `"stud"`, `"perc"`, `"bca"`),
  `parallel` and the number of bootstrap replicates `R`. Defaults are
  `"basic"` for `type`, option `"boot.parallel"` (or `"no"` if unset)
  for `parallel`, and `999` for `R`.

## Value

if `output = "def"` and `conf.level = NA`, a numeric scalar. If
`output = "def"` and a confidence interval is requested, a named numeric
vector with elements:

- `est`:

  point estimate of Krippendorff's alpha

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

If `output = "ext"`, a list with elements:

- `alpha`:

  point estimate of Krippendorff's alpha

- `Do`:

  observed disagreement

- `De`:

  expected disagreement under chance

- `O`:

  coincidence matrix

- `nV`:

  category totals in coincidence space

- `delta2`:

  pairwise distance matrix for the selected metric

- `ci`:

  named numeric vector with `est`, `lci`, and `uci`, or `NA` if no
  interval is requested

## Details

The function constructs the coincidence matrix from the wide-format data
using `CoincidenceFromWide` and then calculates Krippendorff's alpha
based on the chosen distance metric:

- `"nominal"`: Disagreement is 0 if equal, 1 otherwise.

- `"ordinal"`: Squared difference of cumulative proportions.

- `"interval"`: Squared Euclidean distances of scale values.

- `"ratio"`: Squared relative differences of scale values.

## References

Krippendorff, K. (2018). *Content Analysis: An Introduction to Its
Methodology*. Sage Publications.

## See also

[`bootCI`](https://andrisignorell.github.io/lumen/reference/bootCI.html)

Other assoc.agreement: [`ccc()`](ccc.md),
[`cohenKappa()`](cohenKappa.md), [`cronbachAlpha()`](cronbachAlpha.md),
[`icc()`](icc.md), [`kappaM()`](kappaM.md), [`pabak()`](pabak.md),
[`percAgreement()`](percAgreement.md),
[`randolphKappa()`](randolphKappa.md)

## Examples

``` r
# Example with nominal data (3 raters, 5 subjects)
dat <- data.frame(
  r1 = c(1, 2, 1, 3, 2),
  r2 = c(1, 2, 2, 3, 2),
  r3 = c(1, 2, 1, 3, 1)
)
krippAlpha(dat, metric = "nominal")
#> [1] 0.6111111

# Interval-scaled example
dat2 <- data.frame(
  r1 = c(1, 4, 5, 7, 2),
  r2 = c(2, 5, 6, 7, 1),
  r3 = c(1, 4, 6, 6, 2)
)
krippAlpha(dat2, metric = "interval", levels = 1:7)
#> [1] 0.9342105

```
