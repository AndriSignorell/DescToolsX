# Box-Cox Transformation

`boxCox()` applies the Box-Cox transformation to a numeric vector.  
`boxCoxInv()` reverses the transformation.

## Usage

``` r
boxCox(x, lambda, tol = 0.000001)

boxCoxInv(x, lambda, tol = 0.000001)
```

## Arguments

- x:

  a numeric vector. Must contain strictly positive values (except
  `NA`s).

- lambda:

  a single numeric transformation parameter

- tol:

  numeric tolerance for detecting the special case \\\lambda \approx 0\\

## Value

a numeric vector of the same length as `x`. An input consisting only of
`NA` is an error.

## Details

The Box-Cox transformation is defined for strictly positive values of
`x` and is given by

\$\$ f\_\lambda(x) = \left\\ \begin{array}{ll} (x^\lambda - 1) / \lambda
& \mbox{if } \lambda \neq 0 \\ \log(x) & \mbox{if } \lambda = 0
\end{array} \right. \$\$

The transformation requires strictly positive input values. If
`|lambda| < tol`, the logarithmic transformation is used instead for
numerical stability.

The inverse transformation recovers the original data (up to numerical
precision) when the same `lambda` and `tol` are used.

## References

Box, G. E. P. and Cox, D. R. (1964). An analysis of transformations.
*Journal of the Royal Statistical Society, Series B*, **26**(2),
211–252.

## See also

[`boxCoxLambda`](boxCoxLambda.md)

Other transform: [`boxCoxLambda()`](boxCoxLambda.md),
[`logSt()`](logSt.md), [`scaleX()`](scaleX.md),
[`yeoJohnson()`](yeoJohnson.md)

## Examples

``` r
set.seed(1)
x <- rlnorm(500, 1, 0.5)

y <- boxCox(x, lambda = 0.5)
xBack <- boxCoxInv(y, lambda = 0.5)

# check inversion
max(abs(x - xBack))
#> [1] 1.776357e-15

# log-transform (lambda ~ 0)
y0 <- boxCox(x, lambda = 0)
```
