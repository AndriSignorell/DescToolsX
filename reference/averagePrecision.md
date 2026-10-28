# Average Precision Score

Computes average precision (AP) for binary probabilistic predictions.

## Usage

``` r
averagePrecision(x, ...)

# Default S3 method
averagePrecision(x, ref, ...)
```

## Arguments

- x:

  a numeric vector of predicted probabilities or scores. Methods for
  fitted models are provided by the package alloy.

- ...:

  further arguments passed to methods

- ref:

  numeric vector of observed binary outcomes (0/1), the reference the
  predictions are scored against

## Value

A numeric scalar containing the average precision score.

## Details

Average precision summarizes the precision-recall curve as

\$\$AP = \sum_n (R_n - R\_{n-1}) P_n,\$\$

where \\P_n\\ and \\R_n\\ are precision and recall at the \\n\\-th
distinct prediction threshold. Unlike metrics based on predicted class
labels, average precision does not require a classification cutoff.
Prediction values therefore need only be numeric scores; they are not
restricted to the interval \[0, 1\].

## See also

Other model.metrics: [`auc()`](auc.md), [`brierScore()`](brierScore.md),
[`logLoss()`](logLoss.md), [`mae()`](mae.md), [`mape()`](mape.md),
[`mse()`](mse.md), [`nmae()`](nmae.md), [`nmse()`](nmse.md),
[`rmse()`](rmse.md), [`smape()`](smape.md)

## Examples

``` r
resp <- c(0, 0, 1, 1)
pred <- c(0.1, 0.4, 0.35, 0.8)
averagePrecision(pred, resp)
#> [1] 0.8333333
```
