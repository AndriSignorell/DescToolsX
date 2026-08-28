# Describe Relationship: Categorical y vs Numeric x

Computes descriptive statistics for the relationship between a
categorical variable `y` and a numeric variable `x`.

## Usage

``` r
.descQN(y, x, conf.level = 0.95, breaks, right)

# S3 method for class 'Desc.qn'
print(x, verbose = NULL, ...)
```

## Arguments

- x:

  a numeric variable

- verbose:

  amount of printed output

- ...:

  further arguments passed to methods

- y:

  a categorical variable (factor or coercible to factor)

- conf.level:

  confidence level for interval estimates (default 0.95)

- breaks:

  numeric vector defining cut points for `x`. If not supplied, quartiles
  of `x` are used.

- right:

  logical; passed to [`cut()`](https://rdrr.io/r/base/cut.html),
  defining interval closure

## Value

an object of class `c("Desc.qn", "Desc")` with components:

- `grpTable`:

  group-wise summary table

- `kw`:

  result of the Kruskal-Wallis test

- `eta2`:

  effect size

- `levene`:

  result of Levene's test

- `tauB`:

  estimate, confidence interval, and p-value for Kendall's tau-b

- `spearman`:

  estimate, confidence interval, and p-value for Spearman's correlation

- `auc`:

  area under the curve for a binary outcome

- `prevTable`:

  prevalence table for a binary outcome with columns:

  `quantile`

  :   quantile group

  `n`

  :   number of complete cases in the group

  `est`

  :   point estimate of the prevalence

  `lci`

  :   lower confidence interval bound

  `uci`

  :   upper confidence interval bound

- `caTest`:

  result of the Cochran-Armitage test for a binary outcome

## Details

The function summarizes how a numeric variable `x` differs across levels
of a categorical variable `y`.

**Computed statistics**

- Group-wise descriptive statistics (median, IQR, counts)

- Kruskal-Wallis test with effect size (\\\eta^2\\)

- Levene's test for homogeneity of variance

- Kendall's Tau-b with confidence interval and p-value

- Spearman correlation (reported for higher verbosity levels)

**Binary outcomes** If `y` has two levels:

- Area under the curve (AUC)

- Prevalence across quantile groups of `x`

- Cochran-Armitage trend test

**Quantile grouping** The numeric variable `x` is optionally discretized
using `breaks`. By default, quartiles are used.

## See also

[`desc`](Desc.md), [`desc.nn`](Desc.nn.md), [`desc.nq`](desc.nq.md),
[`kruskal.test`](https://rdrr.io/r/stats/kruskal.test.html),
[`leveneTest`](https://andrisignorell.github.io/lumen/reference/leveneTest.html)

Other desc: [`desc()`](Desc.md), [`desc.Date()`](Desc.Date.md),
[`desc.factor()`](Desc.factor.md), [`desc.nn`](Desc.nn.md),
[`desc.nq`](desc.nq.md), [`desc.numeric()`](desc.numeric.md),
[`desc.qq`](desc.qq.md), [`desc.ts()`](desc.ts.md),
[`print.Desc.qq()`](desc.table.md)
