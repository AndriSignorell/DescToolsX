# Association Measures - Common Interface

All association measures in this package share a common interface.

## Arguments

- x:

  either a contingency table, a two-column object (matrix, data.frame or
  list), or a vector of observations (together with `y`)

- y:

  optional second vector. If `x` is not a vector, `y` must be `NULL`.

## Details

The association is defined between two variables that can be provided to
the calculation functions in different ways. If only `x` is passed, this
can either be a matrix, which is then interpreted as a contingency table
(this seems in the case of frequency data the natural interpretation and
is by the way also what
[`chisq.test`](https://rdrr.io/r/stats/chisq.test.html) expects).
However, it can also be a data.frame or a list, which must then contain
exactly 2 elements containing the data. Alternatively, two single data
vectors `x` and `y` can be passed. The two element arguments are
typically processed by forming a contingency table with
[`table`](https://rdrr.io/r/base/table.html)`(x, y, ...)`. `NAs` are by
default handled the same way as the function does, so `NAs` omitted.

If the measure should be calculated pairwise for a set of variables
[`pairApply`](https://andrisignorell.github.io/bedrock/reference/pairApply.html)
can be used. This easily allows to create matrices of association
measures (the same way as the `cor` does). `NAs` again are by default
omitted pairwise, which corresponds to the `pairwise.complete` option of
[`cor`](https://rdrr.io/r/stats/cor.html). Use
[`complete.cases`](https://rdrr.io/r/stats/complete.cases.html), if only
the complete cases of a `data.frame` are to be used. (see examples)

Most functions support calculation of confidence intervals. These can be
requested by setting `conf.level` to the desired value (usually 0.95).
If it is set to `NA`, no confidence interval is computed. One-sided
confidence intervals can be controlled using the `sides` argument. It
names the side on which the *finite* bound lies, which is NOT the
convention used for the alternative hypothesis of a test: `"left"`
yields an interval bounded below and corresponds to an alternative of
`"greater"`. See [ConfidenceIntervals](ConfidenceIntervals.md).
Frequently there is a classic and a bootstrap approach (`"classic"`,
`"boot"`). However most measures have their own specific confidence
intervals methods.

Some association measures define additional parameters such as
`direction`, `base`, or `correct`. Those are documented with the
respective functions.

**Function List**

Following association measures are implemented in **DescToolsX**:

|                                  |                                   |
|----------------------------------|-----------------------------------|
| ` `[cramerV](cramerV.md)         | Cramer's V                        |
| ` `[contCoef](contCoef.md)       | Pearson's Contingency Coefficient |
| ` `[lambda](lambda.md)           | Goodman's Lambda                  |
| ` `[gkTau](gkTau.md)             | Goodman Kruskal's Tau             |
| ` `[gkGamma](ordAssocs.md)       | Goodman Kruskal's Gamma           |
| ` `[kendallTauB](ordAssocs.md)   | Kendall's Tau-b                   |
| ` `[stuartTauC](ordAssocs.md)    | Stuart's Tau-c                    |
| ` `[somersDelta](ordAssocs.md)   | Somers' Delta                     |
| ` `[uncertCoef](uncertCoef.md)   | Theil's Uncertainty Coefficient   |
| ` `[mutInf](mutInf.md)           | Mutual Information                |
| ` `[hoeffdingD](hoeffdingD.md)   | Hoeffding's D                     |
| ` `[corPolychor](corPolychor.md) | Polychoric Correlation            |

## References

Cramer, H. (1946) *Mathematical Methods of Statistics*. Princeton
University Press

Agresti, Alan (1996) *Introduction to categorical data analysis*. NY:
John Wiley and Sons
