# Describe Relationship: Categorical x by Categorical y

Computes descriptive statistics for the relationship between two
categorical variables `x` and `y`.

## Usage

``` r
.descQQ(x, y, ...)
```

## Arguments

- x:

  a categorical variable

- y:

  a categorical variable

- ...:

  further arguments, currently unused

## Value

an object of class `c("Desc.qq", "Desc")`

## Details

This function is a wrapper around [`desc.table`](desc.table.md) applied
to the contingency table `table(x, y)`.

It summarizes the joint distribution of two categorical variables and
provides association measures and visualizations.

**Computed statistics**

- Contingency table

- Row and column percentages

- Association measures (e.g., Cramer's V, Phi)

- Optional statistical tests depending on configuration

**Implementation note** Internally, `desc.qq(x, y)` is equivalent to:


    desc(table(x, y))

## See also

[desc](Desc.md), [desc.table](desc.table.md), [desc.qn](desc.qn.md),
[desc.nn](Desc.nn.md),
[pharos::plot.Desc.table](https://andrisignorell.github.io/pharos/reference/plot.Desc.table.html)

Other desc: [`desc()`](Desc.md), [`desc.Date()`](Desc.Date.md),
[`desc.factor()`](Desc.factor.md), [`desc.nn`](Desc.nn.md),
[`desc.nq`](desc.nq.md), [`desc.numeric()`](desc.numeric.md),
[`desc.qn`](desc.qn.md), [`desc.ts()`](desc.ts.md),
[`print.Desc.qq()`](desc.table.md)
