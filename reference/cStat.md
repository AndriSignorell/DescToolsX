# Concordance Statistic (C-Statistic / AUC)

Computes the concordance statistic (C-statistic), equivalent to the area
under the ROC curve (AUC), for predicted values and a binary outcome.

## Usage

``` r
cStat(x, ...)

# S3 method for class 'glm'
cStat(x, ...)

# Default S3 method
cStat(x, resp, conf.level = NA, ...)
```

## Arguments

- x:

  an object for which the C-statistic should be computed; for the
  default method, a numeric vector of predicted values

- ...:

  additional arguments passed to methods

- resp:

  a binary response vector (numeric, logical, or factor)

- conf.level:

  confidence level for the interval; `NA` (default) suppresses interval
  calculation

## Value

if `conf.level = NA`, an unnamed numeric scalar between 0 and 1;
otherwise a named numeric vector with elements:

- `est`:

  point estimate of the C-statistic.

- `lci`:

  lower confidence interval bound.

- `uci`:

  upper confidence interval bound.

## Details

The C-statistic is defined as the probability that, for a randomly
chosen pair of observations with different outcomes, the observation
with the higher predicted value has the higher observed outcome.

Ties in predicted values are handled by assigning a weight of 0.5.

`resp` is converted with `as.numeric(factor(resp)) - 1`, so the
**second** level in sort order counts as the event - `1` for a 0/1
coding, `TRUE` for a logical, and the second factor level otherwise.
Getting this backwards returns \\1 - C\\ rather than an error, so check
the level order when the response is a factor with unusual labels.

This implementation uses:

- O(n log n) concordance computation

- Parallel bootstrap confidence intervals via RcppParallel

- Efficient memory handling

The number of bootstrap samples can be supplied through `...` as `R`;
the default is 1000.

## Random number generation

A confidence level triggers a bootstrap, which draws a seed from R's
global random number generator and therefore advances it. Call
[`set.seed`](https://rdrr.io/r/base/Random.html) beforehand for
reproducible intervals.

## See also

Other assoc.ordinal: [`conDisPairs()`](conDisPairs.md),
[`kendallW()`](kendallW.md), [`ordAssocs()`](ordAssocs.md)

## Examples

``` r
# Default method
set.seed(1)
x <- runif(100)
y <- rbinom(100, 1, 0.5)
cStat(x, resp = y)
#> [1] 0.4569243

# GLM method
r.mod <- glm(complaint ~ temperature + wrongpizza + wine_ordered,
             data = bedrock::Pizza, family = binomial)
cStat(r.mod, conf.level = 0.95)
#>       est       lci       uci 
#> 0.6251552 0.5786945 0.6705040 
```
