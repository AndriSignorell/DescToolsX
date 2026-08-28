# Brier Score

Computes the Brier score for binary probabilistic predictions,
optionally with a confidence interval via a normal approximation or
bootstrap.

## Usage

``` r
brierScore(
  x,
  pred = NULL,
  conf.level = NA,
  sides = c("two.sided", "left", "right"),
  method = c("normal", "boot"),
  scaled = FALSE,
  ...
)
```

## Arguments

- x:

  either a numeric vector of observed binary outcomes (\\0\\/\\1\\) when
  `pred` is supplied, or a fitted model object (`glm` or similar) from
  which both response and predictions are extracted

- pred:

  a numeric vector of predicted probabilities in \\\[0,1\]\\. Required
  when `x` is a numeric vector; ignored when `x` is a model object.

- conf.level:

  confidence level of the interval. If set to `NA` (the default), only
  the point estimate is returned.

- sides:

  character string specifying the sidedness of the confidence interval
  (one of `"two.sided"` (default), `"left"` or `"right"`). See
  [`ConfidenceIntervals`](ConfidenceIntervals.md).

- method:

  confidence interval method: `"normal"` (delta-method approximation,
  default) or `"boot"` (bootstrap via `brier_boot_cpp()`)

- scaled:

  logical. Should the scaled Brier score be returned? Default `FALSE`.

- ...:

  further arguments passed to the bootstrap engine when
  `method = "boot"`: `R`, `type`, `parallel`, `ncpus`. See Details.

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

`sides` names the side on which the finite bound lies: `"left"` yields
\\\[lci, \infty)\\, `"right"` yields \\(-\infty, uci\]\\.

**Normal interval** (`method = "normal"`)

A delta-method normal approximation based on the variance of the
per-observation Brier losses. Fast and deterministic; reliable for
moderate to large samples. With `scaled = TRUE` the standard error is
carried onto the skill scale by dividing through \\BS\_{\max}\\, which
is treated as fixed; the interval therefore ignores the sampling
variability of the baseline and is mildly anti-conservative. Prefer
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

## See also

[`predict`](https://rdrr.io/r/stats/predict.html)

Other model.metrics: [`auc()`](auc.md), [`mae()`](mae.md),
[`mape()`](mape.md), [`mse()`](mse.md), [`nmae()`](nmae.md),
[`nmse()`](nmse.md), [`rmse()`](rmse.md), [`smape()`](smape.md)

## Examples

``` r
set.seed(1)
resp <- rbinom(200, 1, 0.4)
pred <- plogis(rnorm(200, ifelse(resp == 1, 0.5, -0.5)))

brierScore(resp, pred)
#> [1] 0.2016726
brierScore(resp, pred, conf.level = 0.95)
#>       est       lci       uci 
#> 0.2016726 0.1748892 0.2284559 
brierScore(resp, pred, conf.level = 0.95, method = "boot", type = "bca")
#>       est       lci       uci 
#> 0.2016726 0.1780219 0.2324446 
brierScore(resp, pred, conf.level = 0.95, scaled = TRUE)
#>       est       lci       uci 
#> 0.1693025 0.0589808 0.2796242 
```
