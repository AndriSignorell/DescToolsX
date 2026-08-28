# Impute Missing Values in a Vector

Replaces missing values (`NA`) in a vector by either a summary statistic
computed from the data (e.g. mean, median) or a user-supplied scalar
value.

## Usage

``` r
impute(x, FUN = median, na.rm = TRUE, ...)
```

## Arguments

- x:

  atomic vector

- FUN:

  function used to compute the imputation value, or a scalar value;
  default is `median`

- na.rm:

  logical; whether to remove missing values before computing the summary
  statistic

- ...:

  additional arguments passed to `FUN`

## Value

vector of the same length as `x`, with missing values replaced. The type
follows R's usual coercion rules: imputing a double (the median of an
integer vector, say) into an integer vector returns a double.

## Details

If `FUN` is a function, it is applied to `x` to compute a single
imputation value. If the function accepts an argument named `na.rm`, it
will be passed automatically.

Alternatively, `FUN` can be a single scalar value, which will directly
replace all missing values.

If `FUN` is a function, it must return a single value. An error is
thrown if the returned value is not scalar.

Whether `FUN` accepts `na.rm` is decided from its formal arguments; a
function taking `...` is assumed to pass it on. Functions without either
are called without it.

## See also

For direct value replacement see
[`bedrock::naReplace()`](https://andrisignorell.github.io/bedrock/reference/naReplace.html).

Other impute: [`imputeKnn()`](imputeKnn.md)

## Examples

``` r
x <- c(2, 3, NA, 5, 9)

# Default: median(x, na.rm=TRUE)
impute(x)
#> [1] 2 3 4 5 9

# Using mean(x, na.rm=TRUE)
impute(x, mean)
#> [1] 2.00 3.00 4.75 5.00 9.00

# Using trimmed mean
impute(x, function(x) meanX(x, trim=0.3, na.rm = TRUE))
#> [1] 2 3 4 5 9

# Constant replacement
impute(x, 99)
#> [1]  2  3 99  5  9

# an integer vector stays integer only if the value is one too
impute(c(2L, 3L, NA, 5L), FUN = 4L)
#> [1] 2 3 4 5
```
