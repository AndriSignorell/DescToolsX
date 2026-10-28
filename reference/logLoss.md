# Log Loss

Computes the logarithmic loss (cross-entropy loss) for binary
probabilistic predictions.

## Usage

``` r
logLoss(x, ...)

# Default S3 method
logLoss(x, ref, eps = .Machine$double.eps, ...)
```

## Arguments

- x:

  a numeric vector containing predicted probabilities in \\\[0,1\]\\

- ...:

  further arguments passed to methods

- ref:

  numeric or logical vector containing the observed binary outcomes
  (0/1), the reference the predictions are scored against. Factors are
  not accepted, as the choice of the positive level would have to be
  guessed.

- eps:

  Numeric scalar in \\(0, 0.5)\\ used to clip probabilities away from 0
  and 1. Defaults to `.Machine$double.eps`.

## Value

A numeric scalar containing the log loss.

## Details

Log loss is defined as

\$\$LL = -\frac{1}{n}\sum\_{i=1}^n \left\[y_i \log(\hat p_i) +
(1-y_i)\log(1-\hat p_i)\right\],\$\$

where \\y_i \in \\0,1\\\\ and \\\hat p_i\\ is the predicted probability
of the positive class. Lower values indicate better probabilistic
predictions; a perfect model has log loss 0.

Before taking logarithms the predictions are clipped into the closed
interval \\\[\epsilon, 1-\epsilon\]\\, so that a prediction of exactly 0
or 1 contributes a finite penalty of at most \\-\log(\epsilon)\\ (about
36.0 for the default `eps`) instead of `Inf`.

Being the negative Bernoulli log-likelihood per observation, log loss
equals `deviance(fit) / (2 * n)` for a binomial `glm` fitted to
ungrouped data. Methods for fitted models are provided by the package
alloy.

## See also

Other model.metrics: [`auc()`](auc.md),
[`averagePrecision()`](averagePrecision.md),
[`brierScore()`](brierScore.md), [`mae()`](mae.md), [`mape()`](mape.md),
[`mse()`](mse.md), [`nmae()`](nmae.md), [`nmse()`](nmse.md),
[`rmse()`](rmse.md), [`smape()`](smape.md)

## Examples

``` r
resp <- c(0, 0, 1, 1)
pred <- c(0.1, 0.4, 0.35, 0.8)
logLoss(pred, resp)
#> [1] 0.472288

# a confidently wrong prediction is capped by eps
logLoss(c(1, 0), c(0, 1), eps = 1e-6)
#> [1] 13.81551
```
