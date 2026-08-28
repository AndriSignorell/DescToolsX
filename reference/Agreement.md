# Agreement Measures - Common Interface

All agreement measures in this package share a common interface.

## Arguments

- x:

  matrix or data frame of ratings (subjects x raters), or a single
  vector if `y` is provided

- y:

  optional second vector for two raters

- conf.level:

  confidence level for confidence intervals. If `NA`, no confidence
  interval is computed.

- sides:

  character string specifying the side of the confidence interval, one
  of `"two.sided"` (default), `"left"` or `"right"`. It names the side
  on which the *finite* bound lies, not the direction of an alternative
  hypothesis - see [ConfidenceIntervals](ConfidenceIntervals.md) for the
  full definition and for the difference to DescTools.

- method:

  method used for inference; available methods depend on the measure

- weights:

  optional weighting scheme for ordinal data (e.g. "linear",
  "quadratic")

- model:

  model for the ICC, `"oneway"` or `"twoway"`

- type:

  type of ICC, `"consistency"` or `"agreement"`

- unit:

  unit of the ICC, `"single"` or `"average"`

- ...:

  additional arguments passed to helper functions (e.g. bootstrap
  routines)

## Details

Agreement is defined between two or more raters evaluating the same
subjects. The data can be provided in different formats.

If only `x` is passed, it must be a matrix or data.frame where:

- rows represent subjects

- columns represent raters

Alternatively, for two raters only, two vectors `x` and `y` of equal
length can be supplied.

Missing values (`NA`) are by default omitted pairwise, i.e. only
subjects rated by all involved raters are used. If complete-case
analysis across all raters is required, use
[`complete.cases`](https://rdrr.io/r/stats/complete.cases.html)
beforehand.

Agreement measures depend on:

- Number of raters (2 vs. \>2)

- Scale type (nominal, ordinal, metric)

For ordinal data, weighted approaches are available.

Most functions support confidence intervals via `conf.level`. If
`conf.level = NA`, no interval is computed. One-sided intervals can be
requested via `sides`, which names the side carrying the finite bound;
see [ConfidenceIntervals](ConfidenceIntervals.md).

Inference methods may include classical large-sample approaches or
bootstrap methods ("boot"). Specific measures may provide additional
estimation procedures.

Some agreement measures define additional parameters such as:

- `weights` (for ordinal agreement)

- `model` (for ICC variants)

- `type` (consistency vs. absolute agreement)

- `unit` (single vs. average rating)

**Function List**

Following agreement measures are implemented in **DescToolsX**:

|                                      |                               |
|--------------------------------------|-------------------------------|
| ` `**function**                      | **description**               |
| ` `[cohenKappa](cohenKappa.md)       | Cohen's Kappa                 |
| ` ``scottsPi`                        | Scott's Pi                    |
| ` ``gwetAC1`                         | Gwet's AC1 / AC2              |
| ` `[cronbachAlpha](cronbachAlpha.md) | Cronbach's Alpha              |
| ` `[kappaM](kappaM.md)               | Fleiss' and Light's Kappa     |
| ` `[krippAlpha](krippAlpha.md)       | Krippendorff's Alpha          |
| ` `[kendallW](kendallW.md)           | Kendall's W                   |
| ` `[icc](icc.md)                     | Intraclass Correlation (ICC)  |
| ` `[ccc](ccc.md)                     | Lin's Concordance Correlation |

## References

Cohen, J. (1960) A coefficient of agreement for nominal scales.
Educational and Psychological Measurement.

Fleiss, J. L. (1971) Measuring nominal scale agreement among many
raters. Psychological Bulletin.

Krippendorff, K. (2004) Content Analysis.

Shrout, P. E. and Fleiss, J. L. (1979) Intraclass correlations.

Lin, L. I.-K. (1989) A concordance correlation coefficient.
