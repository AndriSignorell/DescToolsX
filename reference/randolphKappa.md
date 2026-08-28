# Randolph's Free-Marginal Multirater Kappa

Computes Randolph's free-marginal multirater kappa for \\m\\ raters over
\\N\\ subjects. This agreement coefficient does not assume fixed
marginal distributions (i.e., it is free-marginal).

## Usage

``` r
randolphKappa(x, categories = NULL, conf.level = NA, ...)
```

## Arguments

- x:

  a matrix of size \\N \times m\\ with subjects in rows and raters in
  columns; cells contain the assigned categories

- categories:

  the categories a rater could have chosen from, or a single number
  giving how many there were. Defaults to `NULL`, i.e. the categories
  actually observed in `x`. See Details.

- conf.level:

  reserved for future confidence intervals; must be `NA`

- ...:

  reserved for future bootstrap options and currently ignored

## Value

a numeric scalar containing Randolph's kappa

## Details

Let \\k\\ be the number of categories, \\m\\ the number of raters, and
\\N\\ the number of subjects. Randolph's kappa is \$\$\kappa =
\frac{P_o - 1/k}{1 - 1/k},\$\$ where the observed agreement \\P_o\\ is
the proportion of agreeing rater *pairs*, \$\$P_o = \frac{1}{N}
\sum\_{i=1}^{N} \frac{\sum\_{j} n\_{ij}(n\_{ij}-1)}{m(m-1)}.\$\$ Here,
\\n\_{ij}\\ denotes the number of raters who assigned subject \\i\\ to
category \\j\\. This is the same observed agreement as in Fleiss' kappa
(and as computed by [`percAgreement`](percAgreement.md)); Randolph's
coefficient differs from Fleiss' only in the chance agreement \\P_e\\,
which is fixed at \\1/k\\ instead of being estimated from the marginals.

\\P_e = 1/k\\ refers to the categories a rater could have *chosen*, not
to those that happen to occur in the data. If a category was available
but never used, the default `categories = NULL` understates \\k\\ and
thus overstates chance agreement, which biases \\\kappa\\ downwards.
Supply `categories` whenever the coding scheme is known.

Long-format ratings can first be reshaped with
[`raterFrame`](raterFrame.md).

## References

Randolph, J. J. (2005). Free-Marginal Multirater Kappa (multirater
\\\kappa\_{\mathrm{free}}\\): An Alternative to Fleiss’ Fixed-Marginal
Multirater Kappa. Online submission.

## See also

Other assoc.agreement: [`ccc()`](ccc.md),
[`cohenKappa()`](cohenKappa.md), [`cronbachAlpha()`](cronbachAlpha.md),
[`icc()`](icc.md), [`kappaM()`](kappaM.md),
[`krippAlpha()`](krippAlpha.md), [`pabak()`](pabak.md),
[`percAgreement()`](percAgreement.md)

## Examples

``` r
## Matrix (subjects x raters), 5 subjects, 3 raters
x <- matrix(c(
  1,1,1,
  2,2,2,
  1,2,1,
  3,3,3,
  2,2,1
), ncol = 3, byrow = TRUE)
randolphKappa(x)
#> [1] 0.6

# the raters could have chosen from five categories, not just the three
# they used
randolphKappa(x, categories = 5)
#> [1] 0.6666667

## Long format with a formula
df <- data.frame(
  subject = rep(1:5, each = 3),
  rater   = rep(paste0("r", 1:3), times = 5),
  rating  = c(1,1,1, 2,2,2, 1,2,1, 3,3,3, 2,2,1)
)
randolphKappa(raterFrame(rating ~ subject | rater, 
                         data = df, dropSubj=TRUE))
#> [1] 0.6
```
