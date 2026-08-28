# Print a Bland-Altman Analysis

Prints a compact summary of a Bland-Altman agreement analysis including
the estimated bias, limits of agreement, corresponding confidence
intervals, and the number of observations.

## Usage

``` r
# S3 method for class 'BlandAltman'
print(x, digits = 3, ...)
```

## Arguments

- x:

  an object of class `"BlandAltman"` as returned by
  [`blandAltmanData`](blandAltmanData.md)

- digits:

  number of decimal places to display

- ...:

  further arguments passed to or from other methods

## Value

invisibly, `x`
