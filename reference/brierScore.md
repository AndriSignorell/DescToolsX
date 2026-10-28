# Brier Score

Computes the Brier score for binary probabilistic predictions,
optionally with a confidence interval via a normal approximation or
bootstrap.

## Usage

``` r
brierScore(x, ...)

# Default S3 method
brierScore(
  x,
  ref,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  method = c("normal", "boot"),
  scaled = FALSE,
  ...
)
```

## Arguments

- x:

  a numeric vector of predicted probabilities in \\\[0,1\]\\. Methods
  for fitted models are provided by the package alloy.

- ...:

  further arguments passed to the bootstrap engine when
  `method = "boot"`: `R`, `type` (`"perc"`, `"basic"`, `"norm"` or
  `"bca"`; `"stud"` is not available). `parallel` and `ncpus` are
  accepted but have no effect: the resampling loop in `brier_boot_cpp()`
  is serial. See Details.

- ref:

  a numeric vector of observed binary outcomes (\\0\\/\\1\\), the
  reference the predictions are scored against

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals()`](ConfidenceIntervals.md).

- method:

  confidence interval method: `"normal"` (normal approximation for the
  mean loss, default) or `"boot"` (bootstrap via `brier_boot_cpp()`).

- scaled:

  logical. Should the scaled Brier score be returned? Default `FALSE`.

## Value

if `conf.level = NA`, a numeric scalar containing the Brier score;
otherwise a named numeric vector with elements:

- `est`:

  point estimate of the Brier score.

- `lci`:

  lower confidence interval bound.

- `uci`:

  upper confidence interval bound.

## Details

The Brier score is defined as \$\$BS = \frac{1}{n}\sum\_{i=1}^n
\bigl\[y_i(1-\hat p_i)^2 + (1-y_i)\hat p_i^2\bigr\]\$\$ where \\y_i \in
\\0,1\\\\ and \\\hat p_i\\ is the predicted probability. Lower is
better; a perfect model scores 0.

The scaled Brier score (`scaled = TRUE`) expresses skill relative to the
climatological baseline \\BS\_{\max}\\, yielding 1 for a perfect model
and 0 for the no-skill reference.

`sides` names the side on which the finite bound lies. The open side is
reported at the boundary of the score's range, which is \\\[0, 1\]\\ for
the raw and \\(-\infty, 1\]\\ for the scaled score: `"left"` yields
\\\[lci, 1\]\\, `"right"` yields \\\[0, uci\]\\ (raw) or \\(-\infty,
uci\]\\ (scaled). Two-sided intervals are clamped to the same range.
One-sided intervals require `conf.level > 0.5`.

**Normal interval** (`method = "normal"`)

A normal approximation for the mean of the per-observation Brier losses,
with standard error \\s\_{\mathrm{loss}} / \sqrt{n}\\. This
approximation assumes independent observations and is intended for
sufficiently large samples. With `scaled = TRUE`, the standard error is
divided by \\BS\_{\max}\\, treating the estimated baseline as fixed. The
resulting interval does not account for uncertainty in the baseline; its
coverage may therefore differ from the nominal level. Prefer
`method = "boot"` for scaled scores.

**Bootstrap interval** (`method = "boot"`)

Case-resampling bootstrap via `brier_boot_cpp()`. The bootstrap type is
controlled by the `type` argument (passed through `...`):

- `"bca"`:

  bias-corrected and accelerated (default). Most accurate; requires \\R
  \geq 200\\.

- `"perc"`:

  percentile interval

- `"norm"`:

  normal approximation using the bootstrap standard error

Further bootstrap arguments passed through `...` via
`.extractBootArgs()`:

- `R`:

  number of bootstrap replicates (default `999`)

- `parallel`:

  parallelisation: `"no"`, `"multicore"`, or `"snow"` (default `"no"`)

- `ncpus`:

  number of CPUs (default `getOption("boot.ncpus", 1L)`)

## References

Brier, G. W. (1950). Verification of forecasts expressed in terms of
probability. *Monthly Weather Review*, **78**(1), 1–3.
[doi:10.1175/1520-0493(1950)078\<0001:VOFEIT\>2.0.CO;2](https://doi.org/10.1175/1520-0493%281950%29078%3C0001%3AVOFEIT%3E2.0.CO%3B2)

Steyerberg, E. W., Vickers, A. J., Cook, N. R., Gerds, T., Gonen, M.,
Obuchowski, N., Pencina, M. J., and Kattan, M. W. (2010). Assessing the
performance of prediction models: a framework for traditional and novel
measures. *Epidemiology*, **21**(1), 128–138.
[doi:10.1097/EDE.0b013e3181c30fb2](https://doi.org/10.1097/EDE.0b013e3181c30fb2)

Efron, B. (1987). Better bootstrap confidence intervals. *Journal of the
American Statistical Association*, **82**(397), 171–185.
[doi:10.1080/01621459.1987.10478410](https://doi.org/10.1080/01621459.1987.10478410)

## See also

[`stats::predict()`](https://rdrr.io/r/stats/predict.html)

Other model.metrics: [`auc()`](auc.md),
[`averagePrecision()`](averagePrecision.md), [`logLoss()`](logLoss.md),
[`mae()`](mae.md), [`mape()`](mape.md), [`mse()`](mse.md),
[`nmae()`](nmae.md), [`nmse()`](nmse.md), [`rmse()`](rmse.md),
[`smape()`](smape.md)

## Examples

``` r
set.seed(1)
resp <- rbinom(200, 1, 0.4)
pred <- plogis(rnorm(200, ifelse(resp == 1, 0.5, -0.5)))

brierScore(pred, resp)
#> [1] 0.2016726
brierScore(pred, resp, conf.level = 0.95)
#>       est       lci       uci 
#> 0.2016726 0.1748892 0.2284559 
brierScore(pred, resp, conf.level = 0.95, method = "boot", type = "bca")
#>       est       lci       uci 
#> 0.2016726 0.1780219 0.2324446 
brierScore(pred, resp, conf.level = 0.95, scaled = TRUE)
#>       est       lci       uci 
#> 0.1693025 0.0589808 0.2796242 
```
