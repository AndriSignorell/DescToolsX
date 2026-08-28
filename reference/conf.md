# Confusion Matrix and Classification Metrics

Computes confusion matrices and a wide range of performance metrics for
classification models or predicted vs. observed labels.

## Usage

``` r
conf(x, ...)

# S3 method for class 'table'
conf(x, pos = NULL, conf.level = 0.95, ...)

# Default S3 method
conf(x, ref, pos = NULL, na.rm = TRUE, ...)

# S3 method for class 'matrix'
conf(x, pos = NULL, ...)

# S3 method for class 'rpart'
conf(x, ...)

# S3 method for class 'multinom'
conf(x, ...)

# S3 method for class 'glm'
conf(x, cutoff = 0.5, pos = NULL, ...)

# S3 method for class 'randomForest'
conf(x, ...)

# S3 method for class 'svm'
conf(x, ...)

# S3 method for class 'lda'
conf(x, ...)

# S3 method for class 'qda'
conf(x, ...)

# S3 method for class 'Conf'
print(x, digits = max(3L, getOption("digits") - 3L), ...)

# S3 method for class 'Conf'
plot(x, main = "Confusion Matrix", ...)

sensX(x, ...)

specX(x, ...)
```

## Arguments

- x:

  object containing predictions; one of:

  - a factor or character vector of predicted classes

  - a confusion matrix (`table` or `matrix`) with **predictions in the
    rows and references in the columns**

  - a fitted model object (e.g., `glm`, `rpart`)

- ...:

  further arguments passed to specific methods

- pos:

  optional character specifying the positive class (binary
  classification only). If `NULL`, the second level is used and a
  message is issued.

- conf.level:

  confidence level for the accuracy interval; defaults to 0.95

- ref:

  optional reference (true labels). Required for the default method.

- na.rm:

  logical; remove missing values before computation. Default `TRUE`.

- cutoff:

  numeric cutoff for probabilistic models (e.g., `glm`). Default `0.5`.

- digits:

  integer; number of decimal places for printing

- main:

  character string specifying the plot title

## Value

`conf()` returns an object of class `"Conf"` containing:

- `table`:

  confusion matrix

- `pos`:

  positive class (binary only, else `NULL`)

- `diag`:

  number of correct predictions

- `n`:

  total number of observations

- `acc`, `acc.lci`, `acc.uci`:

  accuracy and CI

- `conf.level`:

  confidence level used for the accuracy CI

- `nir`:

  no-information rate

- `acc.pval`:

  p-value for accuracy greater than the no-information rate

- `kappa`:

  Cohen's kappa

- `mcnemar.pval`:

  McNemar test p-value

- `byclass`:

  matrix of class-wise metrics

`sensX()` and `specX()` return a named numeric vector containing the
sensitivity or specificity, respectively, for each reported class.

## Details

This is a generic function with methods for tables, vectors, and several
model objects (e.g., `glm`, `rpart`, `randomForest`, `svm`).

`sensX()` and `specX()` are convenience extractors for the sensitivity
and specificity values computed by `conf()`.

The orientation of the table matters: rows are read as predictions and
columns as references, so the no-information rate is taken from the
column margin. `conf.default()` builds the table accordingly.

**Overall statistics:**

- Accuracy with confidence interval

- No Information Rate (NIR) and p-value (Accuracy \> NIR)

- Cohen's Kappa

- McNemar test p-value

**Class-wise statistics** (computed one-vs-all for multiclass):

- Sensitivity (Recall)

- Specificity

- Positive Predictive Value (Precision)

- Negative Predictive Value

- Prevalence

- Detection Rate and Detection Prevalence

- Balanced Accuracy

- F-value (harmonic mean of Precision and Recall)

- Matthews Correlation Coefficient (MCC)

## Examples

``` r
# vectors
pred <- factor(c("A", "B", "A", "A", "B"))
ref  <- factor(c("A", "A", "A", "B", "B"))
conf(pred, ref)
#> 'pos' not specified, using 'B' as positive class
#> 
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction B A
#>          B 1 1
#>          A 1 2
#> 
#>                 Total n : 5
#>                Accuracy : 0.6000
#>                 95% CI : (0.2307, 0.8824)
#>     No Information Rate : 0.6000
#>     P-Value [Acc > NIR] : 0.683
#>                   Kappa : 0.1667
#>  McNemar's Test P-Value : 1
#> 
#>             Sensitivity : 0.5000
#>             Specificity : 0.6667
#>          Pos Pred Value : 0.5000
#>          Neg Pred Value : 0.6667
#>              Prevalence : 0.4000
#>          Detection Rate : 0.2000
#>    Detection Prevalence : 0.4000
#>       Balanced Accuracy : 0.5833
#>                 F-Value : 0.5000
#>     Matthews Cor.-Coef. : 0.1667
#> 
#>        'Positive' Class : B
#> 

# table
conf(table(pred, ref))
#> 'pos' not specified, using 'B' as positive class
#> 
#> Confusion Matrix and Statistics
#> 
#>     ref
#> pred B A
#>    B 1 1
#>    A 1 2
#> 
#>                 Total n : 5
#>                Accuracy : 0.6000
#>                 95% CI : (0.2307, 0.8824)
#>     No Information Rate : 0.6000
#>     P-Value [Acc > NIR] : 0.683
#>                   Kappa : 0.1667
#>  McNemar's Test P-Value : 1
#> 
#>             Sensitivity : 0.5000
#>             Specificity : 0.6667
#>          Pos Pred Value : 0.5000
#>          Neg Pred Value : 0.6667
#>              Prevalence : 0.4000
#>          Detection Rate : 0.2000
#>    Detection Prevalence : 0.4000
#>       Balanced Accuracy : 0.5833
#>                 F-Value : 0.5000
#>     Matthews Cor.-Coef. : 0.1667
#> 
#>        'Positive' Class : B
#> 

# glm
m <- glm(am ~ hp + wt, data = mtcars, family = binomial)
conf(m)
#> 
#> Confusion Matrix and Statistics
#> 
#>           Reference
#> Prediction  1  0
#>          1 12  1
#>          0  1 18
#> 
#>                 Total n : 32
#>                Accuracy : 0.9375
#>                 95% CI : (0.7985, 0.9827)
#>     No Information Rate : 0.5938
#>     P-Value [Acc > NIR] : < 0.001
#>                   Kappa : 0.8704
#>  McNemar's Test P-Value : 1
#> 
#>             Sensitivity : 0.9231
#>             Specificity : 0.9474
#>          Pos Pred Value : 0.9231
#>          Neg Pred Value : 0.9474
#>              Prevalence : 0.4063
#>          Detection Rate : 0.3750
#>    Detection Prevalence : 0.4063
#>       Balanced Accuracy : 0.9352
#>                 F-Value : 0.9231
#>     Matthews Cor.-Coef. : 0.8704
#> 
#>        'Positive' Class : 1
#> 
```
