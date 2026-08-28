# Percent Agreement with Design-Based SE and CI

Computes the proportion of agreement (percent agreement) along with
design-based standard errors and confidence intervals following Klein
and Gwet.

## Usage

``` r
percAgreement(
  x,
  y = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  input = c("auto", "confusion", "ratings"),
  fpc = 0,
  output = c("def", "ext"),
  ...
)
```

## Arguments

- x:

  a confusion matrix or a matrix or data frame of ratings with subjects
  in rows and raters in columns

- y:

  optional second rating vector used to construct a confusion matrix

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md). A proportion of
  agreement lies in \\\[0, 1\]\\, so the open side is reported at that
  boundary rather than at \\\pm\infty\\.

- input:

  character string specifying the input format: `"auto"`, `"confusion"`,
  or `"ratings"`

- fpc:

  finite population correction, the sampling fraction \\n/N\\ in \\\[0,
  1)\\ (default `0`)

- output:

  output format, either `"def"` (default) or `"ext"` for extended
  results

- ...:

  must be empty. Named arguments are rejected rather than silently
  ignored.

## Value

if `output = "def"` and `conf.level = NA`, a numeric scalar; otherwise a
named numeric vector with elements:

- `est`:

  proportion of agreement

- `lci`:

  lower confidence interval bound

- `uci`:

  upper confidence interval bound

if `output = "ext"`, a list with the elements `est`, `se`, `ci` (the
named triple above), `n`, `nPairable` and `method`.

## Details

For a confusion matrix the agreement indicator of a subject is 1 on the
diagonal and 0 elsewhere. For a ratings matrix the subject-wise
agreement is the proportion of agreeing rater *pairs*, \$\$p\_{o,i} =
\frac{\sum_j n\_{ij}(n\_{ij}-1)}{m_i (m_i - 1)},\$\$ where \\n\_{ij}\\
is the number of raters who assigned subject \\i\\ to category \\j\\ and
\\m_i\\ the number of non-missing ratings for that subject. Subjects
with fewer than two ratings carry no information about agreement and are
excluded (they still count towards \\n\\ in the variance, following
Gwet).

## See also

Other assoc.agreement: [`ccc()`](ccc.md),
[`cohenKappa()`](cohenKappa.md), [`cronbachAlpha()`](cronbachAlpha.md),
[`icc()`](icc.md), [`kappaM()`](kappaM.md),
[`krippAlpha()`](krippAlpha.md), [`pabak()`](pabak.md),
[`randolphKappa()`](randolphKappa.md)
