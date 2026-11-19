# Price and Quantity Indices

Bilateral index numbers comparing prices (or quantities) of a set of
items between a base period 0 and a comparison period 1. The unweighted
(elementary) indices need prices only, the weighted ones use quantities
to weight the items by their economic importance.

## Usage

``` r
priceIndex(
  p0,
  p1,
  q0 = NULL,
  q1 = NULL,
  estimator = c("laspeyres", "paasche", "fisher", "tornqvist", "walsh",
    "marshall-edgeworth", "drobisch", "sato-vartia", "geo-laspeyres", "geo-paasche",
    "palgrave", "jevons", "carli", "dutot"),
  na.rm = FALSE
)

quantityIndex(
  q0,
  q1,
  p0 = NULL,
  p1 = NULL,
  estimator = c("laspeyres", "paasche", "fisher", "tornqvist", "walsh",
    "marshall-edgeworth", "drobisch", "sato-vartia", "geo-laspeyres", "geo-paasche",
    "palgrave", "jevons", "carli", "dutot"),
  na.rm = FALSE
)
```

## Arguments

- p0, p1:

  numeric vectors of prices in the base and in the comparison period.
  Must be non-negative, and strictly positive for estimators built on
  price relatives.

- q0, q1:

  numeric vectors of non-negative quantities in the base and in the
  comparison period, or `NULL` where the estimator does not need them

- estimator:

  the index formula, see Details

- na.rm:

  logical, should items with a missing price or quantity be removed?
  Defaults to `FALSE`, in which case the result is `NA` as soon as one
  value used by the estimator is missing.

## Value

A single unnamed numeric value, the index as a ratio (1 = no change).
`NA` if no items are left or if the index is not defined, e.g. for a
basket without any expenditure.

## Details

All vectors are matched by position: element `i` of `p0`, `p1`, `q0` and
`q1` must refer to the same item. For panels in long format, with items
entering and leaving over time, see [`chainedIndex()`](chainedIndex.md).

Writing \\r_i = p\_{i1}/p\_{i0}\\ for the price relatives and \\s\_{it}
= p\_{it} q\_{it} / \sum_j p\_{jt} q\_{jt}\\ for the expenditure shares,
the estimators are:

- `"laspeyres"`:

  \\\sum p_1 q_0 / \sum p_0 q_0\\, base period basket

- `"paasche"`:

  \\\sum p_1 q_1 / \sum p_0 q_1\\, comparison period basket

- `"fisher"`:

  geometric mean of Laspeyres and Paasche

- `"drobisch"`:

  arithmetic mean of Laspeyres and Paasche

- `"marshall-edgeworth"`:

  basket \\q_0 + q_1\\

- `"walsh"`:

  basket \\\sqrt{q_0 q_1}\\

- `"tornqvist"`:

  \\\prod r_i^{(s\_{i0} + s\_{i1})/2}\\

- `"sato-vartia"`:

  \\\prod r_i^{w_i}\\ with weights proportional to the logarithmic mean
  of \\s\_{i0}\\ and \\s\_{i1}\\

- `"geo-laspeyres"`:

  \\\prod r_i^{s\_{i0}}\\

- `"geo-paasche"`:

  \\\prod r_i^{s\_{i1}}\\

- `"palgrave"`:

  \\\sum s\_{i1} r_i\\

- `"jevons"`:

  unweighted geometric mean of the price relatives

- `"carli"`:

  unweighted arithmetic mean of the price relatives

- `"dutot"`:

  ratio of the unweighted mean prices, \\\sum p_1 / \sum p_0\\

Fisher, Walsh and Törnqvist are superlative indices (Diewert, 1976) and
usually agree closely; Laspeyres and Paasche bracket them in the typical
case of substitution towards items that became relatively cheaper.
Fisher and Sato-Vartia satisfy the factor reversal test: the product of
the price and the quantity index equals the ratio of total expenditure.

All weighted indices are invariant to the scale of the quantities, i.e.
to the size of the population the quantities were observed in. With the
exception of `"marshall-edgeworth"` this also holds if the scale differs
between the two periods.

Estimators that use only one basket need only that basket: `"laspeyres"`
and `"geo-laspeyres"` work without `q1`, `"paasche"`, `"geo-paasche"`
and `"palgrave"` without `q0`. Quantities supplied but not used by the
chosen estimator are ignored.

`quantityIndex()` is the same set of formulas with the roles of prices
and quantities interchanged.

## References

Balk, B. M. (2008). *Price and Quantity Index Numbers: Models for
Measuring Aggregate Change and Difference*. Cambridge University Press.

Diewert, W. E. (1976). Exact and superlative index numbers. *Journal of
Econometrics*, 4(2), 115–145.

ILO, IMF, OECD, UNECE, Eurostat and World Bank (2004). *Consumer Price
Index Manual: Theory and Practice*. International Labour Office, Geneva.

## See also

[`chainedIndex()`](chainedIndex.md) for chained indices over several
periods

Other index.price: [`chainedIndex()`](chainedIndex.md)

## Examples

``` r
p0 <- c(2.0, 5.0, 10.0)
p1 <- c(2.2, 5.5, 9.0)
q0 <- c(100, 40, 10)
q1 <- c(90, 45, 14)

priceIndex(p0, p1, q0)
#> [1] 1.06
## [1] 1.06
priceIndex(p0, p1, q0, q1, estimator = "paasche")
#> [1] 1.048624
## [1] 1.048624
priceIndex(p0, p1, q0, q1, estimator = "tornqvist")
#> [1] 1.054558
## [1] 1.054558
priceIndex(p0, p1, estimator = "jevons")
#> [1] 1.028828
## [1] 1.028828

# factor reversal: price index x quantity index = expenditure ratio
priceIndex(p0, p1, q0, q1, estimator = "fisher") *
  quantityIndex(q0, q1, p0, p1, estimator = "fisher")
#> [1] 1.143
## [1] 1.143
sum(p1 * q1) / sum(p0 * q0)
#> [1] 1.143
## [1] 1.143
```
