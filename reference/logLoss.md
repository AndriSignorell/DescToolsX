# Log Loss

Computes the logarithmic loss (cross-entropy loss) for binary
probabilistic predictions.

## Usage

``` r
logLoss(x, pred = NULL, eps = .Machine$double.eps)
```

## Arguments

- x:

  Either a numeric or logical vector containing the observed binary
  outcomes (0/1) when `pred` is supplied, or a fitted binomial `glm`
  from which response and fitted probabilities are extracted. Factors
  are not accepted, as the choice of the positive level would have to be
  guessed.

- pred:

  Numeric vector containing predicted probabilities in \\\[0,1\]\\.
  Required when `x` is a response vector.

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

Being the negative Bernoulli log-likelihood per observation, log loss is
extracted automatically only from a `glm` with family `"binomial"`
fitted to ungrouped data (all prior weights equal to one); for such a
fit `logLoss(x)` equals `deviance(x) / (2 * n)`. For every other model
class, pass response and predicted probabilities explicitly.

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
logLoss(resp, pred)
#> [1] 0.472288

# a confidently wrong prediction is capped by eps
logLoss(c(0, 1), c(1, 0), eps = 1e-6)
#> [1] 13.81551

m <- glm(am ~ hp + wt, data = mtcars, family = binomial)
logLoss(m)
#> [1] 0.1571736

# equivalently, half the mean deviance
m$deviance / (2 * nrow(mtcars))
#> [1] 0.1571736
```
