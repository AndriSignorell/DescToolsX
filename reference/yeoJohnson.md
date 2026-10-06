# Yeo-Johnson Transformation

`yeoJohnson()` applies the Yeo-Johnson transformation to a numeric
vector.  
`yeoJohnsonInv()` reverses the transformation.

## Usage

``` r
yeoJohnson(x, lambda, tol = 0.000001)

yeoJohnsonInv(x, lambda, tol = 0.000001)
```

## Arguments

- x:

  a numeric vector

- lambda:

  a single numeric transformation parameter

- tol:

  numeric tolerance for detecting the special cases \\\lambda \approx
  0\\ and \\\lambda \approx 2\\

## Value

a numeric vector of the same length as `x`. Names and dimensions of `x`
are preserved.

## Details

The Yeo-Johnson transformation extends the Box-Cox transformation to
allow for zero and negative values. It is defined piecewise:

\$\$f\_\lambda(x) = \frac{(x+1)^\lambda - 1}{\lambda}\$\$ for \\x \ge
0\\ and \\\lambda \ne 0\\,

\$\$f\_\lambda(x) = \log(x+1)\$\$ for \\x \ge 0\\ and \\\lambda = 0\\,

\$\$f\_\lambda(x) = -\frac{(-x+1)^{2-\lambda} - 1}{2-\lambda}\$\$ for
\\x \< 0\\ and \\\lambda \ne 2\\, and

\$\$f\_\lambda(x) = -\log(-x+1)\$\$ for \\x \< 0\\ and \\\lambda = 2\\.

The transformation is defined for all real-valued inputs and is
continuous and differentiable for all \\x\\. It is commonly used as an
alternative to the Box-Cox transformation when the data include zero or
negative values.

The transformation is strictly increasing and maps 0 to 0, so the sign
of the transformed value identifies the branch to be inverted.

The inverse transformation recovers the original data (up to numerical
precision) when the same `lambda` and `tol` are used. For \\\lambda \<
0\\ the image of the transformation is bounded above by \\-1/\lambda\\
(and correspondingly for \\\lambda \> 2\\ below); values outside that
range have no preimage and are reported as an error.

## References

Yeo, I.-K. and Johnson, R. A. (2000). A new family of power
transformations to improve normality or symmetry. *Biometrika*,
**87**(4), 954–959.

## See also

Other transform: [`boxCox()`](boxCox.md),
[`boxCoxLambda()`](boxCoxLambda.md), [`logSt()`](logSt.md),
[`scaleX()`](scaleX.md)

## Examples

``` r
set.seed(1)
x <- rnorm(500)

y <- yeoJohnson(x, lambda = 0.5)
x_back <- yeoJohnsonInv(y, lambda = 0.5)

# Check inversion
max(abs(x - x_back))
#> [1] 1.332268e-15

# Compare with log-like transformation
y0 <- yeoJohnson(x, lambda = 0)

```
