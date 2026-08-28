# Kendall's Coefficient of Concordance W

Computes Kendall's coefficient of concordance, a popular measure of
association. It is an index of interrater reliability of ordinal data.
The coefficient could be corrected for ties within raters.

## Usage

``` r
kendallW(x, correct = FALSE, test = FALSE, na.rm = NULL)
```

## Arguments

- x:

  \\n \times m\\ matrix or dataframe, k subjects (in rows) m raters (in
  columns)

- correct:

  a logical indicating whether the coefficient should be corrected for
  ties within raters (default `FALSE`)

- test:

  a logical indicating whether the test statistic and p-value should be
  reported (default `FALSE`)

- na.rm:

  deprecated and ignored

## Value

if `test = FALSE`, a numeric scalar containing Kendall's W. Otherwise an
object of class `"htest"` with components:

- `statistic`:

  chi-squared test statistic

- `p.value`:

  p-value for the test

- `method`:

  description of the test and coefficient variant

- `data.name`:

  name of the data

- `estimate`:

  coefficient of concordance W

- `parameter`:

  degrees of freedom and numbers of subjects and raters

## Details

The test for Kendall's W is completely equivalent to
[`friedman.test`](https://rdrr.io/r/stats/friedman.test.html). The only
advantage of this test over Friedman's is that Kendall's W has an
interpretation as the coefficient of concordance. The test itself is
only valid for large samples.  
Kendall's W should be corrected for ties, if raters did not use a true
ranking order for the subjects. The function warns if ties are present
and no correction has been required.

In the presence of `NAs` the algorithm is switched to a generalized form
for randomly incomplete datasets introduced in Brueckl (2011). This
approach uses the mean Spearman \\\rho\\ of all pairwise comparisons
(see Kendall, 1962):  

\$\$W = (1+mean(\rho)\*(k-1)) / k\$\$

where k is the mean number of (pairwise) ratings per object and
mean(\\\rho\\) is calculated weighted, according to Taylor (1987), since
the pairwise are possibly based on a different number of ratings, what
must be reflected in weights. In case of complete datasets, it yields
the same results as usual implementations of Kendall's W, except for
tied ranks. In case of tied ranks, the (pairwise) correction of s used,
which (already with complete datasets) results in slightly different
values than the tie correction explicitly specified for W.

## Note

Based on code by Matthias Gamer and Markus Brueckl, adapted to conform
to package standards.

## References

Kendall, M.G. (1948) *Rank correlation methods*. London: Griffin.

Kendall, M.G. (1962). Rank correlation methods (3rd ed.). London:
Griffin.

Brueckl, M. (2011). Statistische Verfahren zur Ermittlung der
Urteileruebereinstimmung. in: Altersbedingte Veraenderungen der Stimme
und Sprechweise von Frauen, Berlin: Logos, 88-103.

Taylor, J.M.G. (1987). Kendall's and Spearman's correlation coefficients
in the presence of a blocking variable. *Biometrics*, 43, 409-416.

## See also

[stats::cor](https://rdrr.io/r/stats/cor.html),
[stats::friedman.test](https://rdrr.io/r/stats/friedman.test.html)

Other assoc.ordinal: [`cStat()`](cStat.md),
[`conDisPairs()`](conDisPairs.md), [`ordAssocs()`](ordAssocs.md)

## Examples

``` r

anxiety <- data.frame(rater1=c(3,3,3,4,5,5,2,3,5,2,2,6,1,5,2,2,1,2,4,3),
                      rater2=c(3,6,4,6,2,4,2,4,3,3,2,3,3,3,2,2,1,3,3,4),
                      rater3=c(2,1,4,4,3,2,1,6,1,1,1,2,3,3,1,1,3,3,2,2))

kendallW(anxiety, TRUE)
#> [1] 0.5396569

# with test results
kendallW(anxiety, TRUE, test=TRUE)
#> 
#>  Kendall's coefficient of concordance W (with ties correction)
#> 
#> data:  anxiety
#> Kendall chi-squared = 30.76, df = 19, subjects = 20, raters = 3,
#> p-value = 0.04288
#> alternative hypothesis: W is greater 0
#> sample estimates:
#>         W 
#> 0.5396569 
#> 

# example from Siegel and Castellan (1988)
d.att <- data.frame(
  id        = c(4,21,11),
  airfare   = c(5,1,4),
  climate   = c(6,7,5),
  season    = c(7,6,1),
  people    = c(1,2,3),
  program   = c(2,3,2),
  publicity = c(4,5,7),
  present   = c(3,4,6),
  interest  = c(8,8,8)
)

kendallW(t(d.att[, -1]), test = TRUE)
#> 
#>  Kendall's coefficient of concordance W
#> 
#> data:  t(d.att[, -1])
#> Kendall chi-squared = 13.778, df = 7, subjects = 8, raters = 3, p-value
#> = 0.05528
#> alternative hypothesis: W is greater 0
#> sample estimates:
#>         W 
#> 0.6560847 
#> 

# which is perfectly the same as
friedman.test(y=as.matrix(d.att[,-1]), groups = d.att$id)
#> 
#>  Friedman rank sum test
#> 
#> data:  as.matrix(d.att[, -1])
#> Friedman chi-squared = 13.778, df = 7, p-value = 0.05528
#> 

```
