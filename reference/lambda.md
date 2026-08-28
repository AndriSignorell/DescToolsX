# Goodman Kruskal Lambda

Calculate symmetric and asymmetric Goodman Kruskal lambda and their
confidence intervals. Lambda is a measure of proportional reduction in
error in cross tabulation analysis. For any sample with a nominal
independent variable and dependent variable (or ones that can be treated
nominally), it indicates the extent to which the modal categories and
frequencies for each value of the independent variable differ from the
overall modal category and frequency, i.e. for all values of the
independent variable together

## Usage

``` r
lambda(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  direction = c("symmetric", "row", "column"),
  ...
)
```

## Arguments

- x:

  either a contingency table, a two-column object (matrix, data.frame or
  list), or a vector of observations (together with `y`)

- y:

  optional second vector. If `x` is not a vector, `y` must be `NULL`.

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See details
  in [`ConfidenceIntervals`](ConfidenceIntervals.md).

- direction:

  type of lambda. Can be one out of `"symmetric"` (default), `"row"`,
  `"column"` (abbreviations are allowed). If direction is set to `"row"`
  then lambda(R\|C) (column dependent) will be reported. See Details.

- ...:

  further arguments, passed on to
  [`normalizeToConfusion`](normalizeToConfusion.md) and
  [`table`](https://rdrr.io/r/base/table.html) for building the table -
  `useNA` is the usual one.

## Value

if `conf.level = NA`, a numeric scalar. Otherwise a named numeric vector
with elements:

- `est`:

  point estimate of Goodman–Kruskal lambda

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

Asymmetric lambda is interpreted as the probable improvement in
predicting the column variable Y given knowledge of the row variable
X.  
The nondirectional lambda is the average of the two asymmetric lambdas,
lambda(C\|R) and lambda(R\|C). lambda (asymmetric and symmetric) has a
scale ranging from 0 to 1.

## Note

Based on code by Antti Arppe and Nanina Anderegg (confidence interval
symmetric lambda), adapted to conform to package standards.

## References

Agresti, A. (2002) *Categorical Data Analysis*. John Wiley & Sons

Goodman, L. A., Kruskal W. H. (1979) Measures of Association for Cross
Classifications. New York: Springer-Verlag (contains articles appearing
in *J. Amer. Statist. Assoc.* in 1954, 1959, 1963, 1972).  
http://www.nssl.noaa.gov/users/brooks/public_html/feda/papers/goodmankruskal1.pdf
(might be outdated)

Liebetrau, A. M. (1983) *Measures of Association*, Sage University
Papers Series on Quantitative Applications in the Social Sciences,
07-004. Newbury Park, CA: Sage, pp. 17–24

## See also

[`Association`](Association.md)

Other assoc.nominal: [`contCoef()`](contCoef.md),
[`cramerV()`](cramerV.md), [`gkTau()`](gkTau.md),
[`mutInf()`](mutInf.md), [`phi()`](phi.md),
[`tschuprowT()`](tschuprowT.md), [`uncertCoef()`](uncertCoef.md),
[`yule`](yule.md)

## Examples

``` r

# example from Goodman Kruskal (1954)
m <- as.table(cbind(c(1768,946,115), c(807,1387,438), c(189,746,288), c(47,53,16)))
dimnames(m) <- list(paste("A", 1:3), paste("B", 1:4))
m
#>      B 1  B 2  B 3  B 4
#> A 1 1768  807  189   47
#> A 2  946 1387  746   53
#> A 3  115  438  288   16

# direction default is "symmetric"
lambda(m)
#> [1] 0.2076188
lambda(m, conf.level=0.95)
#>       est       lci       uci 
#> 0.2076188 0.1871747 0.2280629 

lambda(m, direction="row")
#> [1] 0.2241003
lambda(m, direction="column")
#> [1] 0.1923949
```
