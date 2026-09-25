# Describe Relationship: Numeric x by Categorical g

Computes descriptive statistics for a numeric variable `x` grouped by a
categorical variable `g`.

## Usage

``` r
.descNQ(x, g, ...)

# S3 method for class 'Desc.nq'
print(x, digits = NULL, ...)

# S3 method for class 'Desc.nq'
plot(x, main = x$meta$main, which = NULL, ...)
```

## Arguments

- x:

  a numeric variable

- digits:

  number of digits used to format relative frequencies

- ...:

  further arguments, currently unused

- main:

  main title for the plot; defaults to the title stored in `x$meta$main`

- which:

  integer vector selecting which plots to draw. See Details. `NULL`
  (default) selects plots automatically based on `verbose`.

- g:

  a categorical grouping variable (factor or coercible to factor)

## Value

an object of class `c("Desc.nq", "Desc")` with components:

- `tab`:

  group-wise summary table

- `test`:

  result of the Kruskal-Wallis test

- `vtest`:

  result of Levene's test

- `eta`:

  effect size

## Details

The function summarizes the distribution of `x` across levels of `g` and
performs nonparametric tests of group differences.

**Computed statistics**

- Group-wise descriptive statistics (mean, median, SD, IQR, counts)

- Kruskal-Wallis test

- Effect size (\\\eta^2\\) based on the Kruskal-Wallis statistic

- Levene's test for homogeneity of variance

**Interpretation** The Kruskal-Wallis test evaluates whether the
distribution of `x` differs between groups defined by `g`. The effect
size \\\eta^2\\ provides a standardized measure of group differences.

## See also

[desc](Desc.md), [desc.qn](desc.qn.md), [desc.nn](Desc.nn.md),
[pharos::plot.Desc.qn](https://andrisignorell.github.io/pharos/reference/plot.Desc.qn.html)
[kruskal.test](https://rdrr.io/r/stats/kruskal.test.html),
[lumen::leveneTest](https://andrisignorell.github.io/lumen/reference/leveneTest.html)

Other desc: [`desc()`](Desc.md), [`desc.Date()`](Desc.Date.md),
[`desc.factor()`](Desc.factor.md), [`desc.nn`](Desc.nn.md),
[`desc.numeric()`](desc.numeric.md), [`desc.qn`](desc.qn.md),
[`desc.qq`](desc.qq.md), [`desc.ts()`](desc.ts.md),
[`print.Desc.qq()`](desc.table.md)
