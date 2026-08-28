# Effect Size Calculations for ANOVAs

Calculates eta-squared, partial eta-squared and generalized eta-squared

## Usage

``` r
etaSq(fit, type = 2, anova = FALSE)

# S3 method for class 'lm'
etaSq(fit, type = 2, anova = FALSE)

# S3 method for class 'aovlist'
etaSq(fit, type = 2, anova = FALSE)

aovlDetails(fit)

aovlErrorTerms(fit)
```

## Arguments

- fit:

  an analysis of variance object of class `"aov"` or `"aovlist"`

- type:

  type of sums of squares to calculate. `etaSq.aovlist()` requires
  `type = 1`.

- anova:

  logical; whether to include the full ANOVA table with the effect sizes

## Value

for `etaSq.lm()`, a numeric matrix with one row per model term and
columns `eta.sq` and `eta.sq.part`; for `etaSq.aovlist()`, a numeric
matrix that additionally contains `eta.sq.gen`. If `anova = TRUE`, ANOVA
statistics are included in additional columns. `aovlDetails()` returns a
data frame of effect terms and their ANOVA statistics, while
`aovlErrorTerms()` returns a list with components:

- `SS`:

  error sums of squares

- `MS`:

  error mean squares

- `DF`:

  error degrees of freedom

## Details

Calculates the eta-squared, partial eta-squared, and generalized
eta-squared measures of effect size that are commonly used in analysis
of variance. The input `fit` should be the analysis of variance object
itself. For between-subjects designs, generalized eta-squared equals
partial eta-squared. The reported generalized eta-squared for
repeated-measures designs assumes that all factors are manipulated,
i.e., that there are no measured factors like gender (see references).

For unbalanced designs, the default in `etaSq` is to compute Type II
sums of squares (`type=2`), in keeping with the `Anova` function in the
`car` package. It is possible to revert to the Type I SS values
(`type=1`) to be consistent with `anova`, but this rarely tests
hypotheses of interest. Type III SS values (`type=3`) can also be
computed. `etaSq.aovlist` requires `type=1`.

## Note

Based on code by Danielle Navarro, and Daniel Wollschlaeger.

## References

Bakeman, R. (2005). Recommended effect size statistics for repeated
measures designs. Behavior Research Methods 37(3), 379-384.

Olejnik, S. and Algina, J. (2003). Generalized Eta and Omega Squared
Statistics: Measures of Effect Size for Some Common Research Designs.
Psychological Methods 8(4), 434-447.

## See also

[`aov`](https://rdrr.io/r/stats/aov.html),
[`anova`](https://rdrr.io/r/stats/anova.html), `Anova`

Other effect.size: [`cohenD()`](cohenD.md), [`cohenH()`](cohenH.md),
[`glassDelta()`](glassDelta.md), [`oddsRatio()`](oddsRatio.md),
[`relRisk()`](relRisk.md)

## Examples

``` r

#### Example 1: one-way ANOVA ####

outcome <- c(1.4,2.1,3.0,2.1,3.2,4.7,3.5,4.5,5.4)    # data
treatment1 <- factor(c(1,1,1,2,2,2,3,3,3))           # grouping variable
anova1 <- aov(outcome ~ treatment1)                  # run the ANOVA
summary(anova1)                                      # print the ANOVA table
#>             Df Sum Sq Mean Sq F value Pr(>F)  
#> treatment1   2  7.936   3.968   3.663 0.0913 .
#> Residuals    6  6.500   1.083                 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
etaSq(anova1)                                        # effect size
#>               eta.sq eta.sq.part
#> treatment1 0.5497229   0.5497229

#### Example 2: two-way ANOVA ####

treatment2 <- factor(c(1,2,3,1,2,3,1,2,3))       # second grouping variable
anova2 <- aov(outcome ~ treatment1 + treatment2) # run the ANOVA
summary(anova2)                                  # print the ANOVA table
#>             Df Sum Sq Mean Sq F value  Pr(>F)   
#> treatment1   2  7.936   3.968    55.8 0.00120 **
#> treatment2   2  6.216   3.108    43.7 0.00191 **
#> Residuals    4  0.284   0.071                   
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
etaSq(anova2)                                    # effect size
#>               eta.sq eta.sq.part
#> treatment1 0.5497229   0.9653961
#> treatment2 0.4305727   0.9562393

#### Example 3: two-way ANOVA unbalanced cell sizes ####
#### data from Maxwell & Delaney, 2004              ####
#### Designing experiments and analyzing data       ####

dfMD <- data.frame(IV1=factor(rep(1:3, c(3+5+7, 5+6+4, 5+4+6))),
                   IV2=factor(rep(rep(1:3, 3), c(3,5,7, 5,6,4, 5,4,6))),
                   DV=c(c(41, 43, 50), c(51, 43, 53, 54, 46), c(45, 55, 56, 60, 58, 62, 62),
                        c(56, 47, 45, 46, 49), c(58, 54, 49, 61, 52, 62), c(59, 55, 68, 63),
                        c(43, 56, 48, 46, 47), c(59, 46, 58, 54), c(55, 69, 63, 56, 62, 67)))

# use contr.sum for correct sum of squares type 3
dfMD$IV1s <- C(dfMD$IV1, "contr.sum")
dfMD$IV2s <- C(dfMD$IV2, "contr.sum")
dfMD$IV1t <- C(dfMD$IV1, "contr.treatment")
dfMD$IV2t <- C(dfMD$IV2, "contr.treatment")

etaSq(aov(DV ~ IV1s*IV2s, data=dfMD), type=3)
#>                eta.sq eta.sq.part
#> IV1s      0.086255016  0.16919863
#> IV2s      0.497535493  0.54017354
#> IV1s:IV2s 0.005976273  0.01391427
etaSq(aov(DV ~ IV1t*IV2t, data=dfMD), type=1)
#>                eta.sq eta.sq.part
#> IV1t      0.042592627  0.09137634
#> IV2t      0.527900579  0.55484898
#> IV1t:IV2t 0.005976273  0.01391427

#### Example 4: two-way split-plot ANOVA -> etaSq.aovlist ####

set.seed(1)
DV_t1 <- round(rnorm(3*10, -0.5, 1), 2)
DV_t2 <- round(rnorm(3*10,  0,   1), 2)
DV_t3 <- round(rnorm(3*10,  0.5, 1), 2)
dfSPF <- data.frame(id=factor(rep(1:(3*10), times=3)),
                    IVbtw=factor(rep(LETTERS[1:3], times=3*10)),
          IVwth=factor(rep(1:3, each=3*10)),
          DV=c(DV_t1, DV_t2, DV_t3))
spf <- aov(DV ~ IVbtw*IVwth + Error(id/IVwth), data=dfSPF)
etaSq(spf, type=1, anova=TRUE)
#>                 eta.sq eta.sq.part eta.sq.gen         SS df        MS      SSE
#> IVbtw       0.01359217  0.03775582 0.01693141  1.1645622  2 0.5822811 29.68002
#> IVwth       0.18569613  0.29547219 0.19048057 15.9102422  2 7.9551211 37.93659
#> IVbtw:IVwth 0.01152556  0.02536984 0.01439415  0.9874978  4 0.2468744 37.93659
#>             dfE          F            p
#> IVbtw        27  0.5297029 5.947733e-01
#> IVwth        54 11.3235402 7.820776e-05
#> IVbtw:IVwth  54  0.3514079 8.419225e-01
```
