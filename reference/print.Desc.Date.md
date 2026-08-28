# Print method for `"Desc.Date"` objects

Prints a structured summary of a `"Desc.Date"` object as created by
[`desc`](Desc.md). The output includes core time-axis statistics such as
range, span, coverage and fundamental quantiles.

## Usage

``` r
# S3 method for class 'Desc.Date'
print(x, verbose = NULL, ...)
```

## Arguments

- x:

  an object of class `"Desc.Date"`

- verbose:

  integer controlling the amount of printed detail. If `NULL`, the
  verbosity stored in the object metadata is used.

  0–1

  :   core statistics only (range, span, coverage, quantiles, and
      sentinel detection)

  2

  :   additionally prints weekday distribution with observed counts,
      expected counts, standardized residuals and chi-square p-value

  3

  :   additionally prints month distribution

- ...:

  further arguments passed to underlying print methods

## Details

Depending on the selected verbosity level, weekday and month
distributions are shown including observed and expected counts,
standardized residuals and chi-square p-values.

Standardized residuals are displayed together with a directional
deviation marker using "+" and "-" symbols. These markers reflect the
magnitude and direction of deviation from expectation but do not
represent separate hypothesis tests.

Weekday and month distributions are compared to their expected
probabilities as defined in [`desc`](Desc.md).

Standardized residuals are computed as \$\$(Observed - Expected) /
sqrt(Expected)\$\$. Larger absolute values indicate stronger deviations
from the expected distribution.

Sentinel values (e.g., extreme future dates such as 3000-01-01 or
implausibly early dates) are reported as potential data-quality issues.
