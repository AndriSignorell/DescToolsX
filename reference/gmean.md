# Geometric Mean and Standard Deviation

Calculates the geometric mean, its confidence interval and the geometric
standard deviation of a vector x.

## Usage

``` r
gmean(
  x,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  method = c("classic", "boot"),
  na.rm = FALSE,
  ...
)

gsd(x, na.rm = FALSE)
```

## Arguments

- x:

  a positive numeric vector. An object which is not a vector is coerced,
  if possible, by [`as.vector()`](https://rdrr.io/r/base/vector.html).

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- method:

  a vector of character strings representing the type of intervals
  required. The value should be any subset of the values `"classic"`,
  `"boot"`. See [`boot.ci`](https://rdrr.io/pkg/boot/man/boot.ci.html).

- na.rm:

  logical, indicating whether `NA` values should be stripped before the
  computation proceeds. Defaults to `FALSE`.

- ...:

  further arguments are passed to the
  [`boot`](https://rdrr.io/pkg/boot/man/boot.html) function. Supported
  arguments are `type` (`"norm"`, `"basic"`, `"stud"`, `"perc"`,
  `"bca"`), `parallel` and the number of bootstrap replicates `R`. If
  not defined those will be set to their defaults, being `"basic"` for
  `type`, option `"boot.parallel"` (and if that is not set, `"no"`) for
  `parallel` and `999` for `R`.

## Value

for `gsd()`, a numeric scalar (`NA` if fewer than two strictly positive
values remain). For `gmean()`, a numeric scalar if `conf.level = NA`;
otherwise a named numeric vector with elements:

- `est`:

  point estimate of the geometric mean

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

## Details

The geometric mean is defined as: \$\$\sqrt\[n\]{x\_{1}\cdot x\_{2}\cdot
x\_{3} \ldots \cdot x\_{n}}\$\$

The geometric mean and geometric standard deviation are restricted to
positive inputs (because otherwise the answer can have an imaginary
component). Hence if any argument is negative, the result will be `NA`.
If any argument is zero, then the geometric mean is zero.  
For strict positive values the geometric mean is computed as
`exp(meanCI(log(x)))`.

**Considerations (Roenfeldt 2018)** ` ` "The calculation of the
geometric mean requires that all values are non-zero and positive. So
what should you do if you have data that do not meet this requirement?
If you have values that equal zero, you have a few options:

- Adjust your scale so that you add 1 to every number in the data set,
  and then subtract 1 from the resulting geometric mean.

- Ignore zeros or missing data in your calculations.

- Convert zeros to a very small number (often called "below the
  detection limit") that is less than the next smallest number in the
  data set.

If you have negative numbers, you will need to convert those numbers to
a positive value before calculating the geometric mean. You can then
assign the resulting geometric mean a negative value. If your data set
contains both positive and negative values, you will have to separate
them and find the geometric means for each group, and you can then find
the weighted average of their individual geometric means to find the
total geometric mean for the full data set. If none of these options
appeals to you, you are not alone! There is controversy among
statisticians about what is the best method for dealing with these
values. You may want to calculate several types of averages and decide
what makes the most sense for you and the results you are trying to
report."

## References

Snedecor, G. W., Cochran, W. G. Cochran (1989) Statistical Methods, 8th
ed. Ames, *IA: Iowa State University Press*

Roenfeldt K. (2018) Better than Average: Calculating Geometric Means
Using SAS, Henry M. Jackson Foundation for the Advancement of Military
Medicine,  
<https://www.lexjansen.com/wuss/2018/56_Final_Paper_PDF.pdf>

## See also

Other location: [`hmean()`](hmean.md),
[`hodgesLehmann()`](hodgesLehmann.md), [`huberM()`](huberM.md),
[`meanX()`](meanX.md), [`medianX()`](medianX.md), [`modeX()`](modeX.md),
[`tukeyBiweight()`](tukeyBiweight.md)

## Examples

``` r
set.seed(1)
x <- runif(5)
gmean(x)
#> [1] 0.4009889

m <- matrix(runif(50), nrow = 10)
apply(m, 2, gmean)
#> [1] 0.4175376 0.4631122 0.3242875 0.5392261 0.3268179

sapply(as.data.frame(m), gmean)
#>        V1        V2        V3        V4        V5 
#> 0.4175376 0.4631122 0.3242875 0.5392261 0.3268179 


# example in https://www.stata.com/manuals13/rameans.pdf
x <- c(5,4,-4,-5,0,0,NA,7)

# positives only
gmean(x[x>0], na.rm=TRUE, conf.level=0.95)
#>       est       lci       uci 
#>  5.192494  2.578990 10.454477 

# shift by 5 so that everything is positive, then drop the zeros:
# naReplace() puts zeros IN (turning NA into 0), which makes the
# geometric mean collapse to 0 - the opposite of what is wanted here
z <- x + 5
gmean(z[!is.na(z) & z > 0], conf.level = 0.95)
#>       est       lci       uci 
#>  5.477226  2.109600 14.220706 
```
