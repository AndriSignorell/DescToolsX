# Confusion Matrix and Classification Metrics

Computes confusion matrices and a wide range of performance metrics for
classification models or predicted vs. observed labels.

## Usage

``` r
confusion(x, ...)

# S3 method for class 'table'
confusion(x, pos = NULL, conf.level = 0.95, ...)

# Default S3 method
confusion(x, ref, pos = NULL, na.rm = FALSE, ...)

# S3 method for class 'matrix'
confusion(x, pos = NULL, ...)

# S3 method for class 'rpart'
confusion(x, ...)

# S3 method for class 'multinom'
confusion(x, ...)

# S3 method for class 'glm'
confusion(x, cutoff = 0.5, pos = NULL, ...)

# S3 method for class 'randomForest'
confusion(x, ...)

# S3 method for class 'svm'
confusion(x, ...)

# S3 method for class 'lda'
confusion(x, ...)

# S3 method for class 'qda'
confusion(x, ...)

# S3 method for class 'Confusion'
print(x, digits = max(3L, getOption("digits") - 3L), ...)

# S3 method for class 'Confusion'
plot(x, main = "Confusion Matrix", ...)

sensitivity(x, ...)

specificity(x, ...)
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

  logical; if `TRUE`, pairs with a missing prediction or reference are
  removed before computation. With the default `FALSE` such pairs make
  all statistics `NA`; the table of the complete pairs is still
  returned.

- cutoff:

  numeric cutoff for probabilistic models (e.g., `glm`). Default `0.5`.

- digits:

  integer; number of decimal places for printing

- main:

  character string specifying the plot title

## Value

`confusion()` returns an object of class `"Confusion"` containing:

- `table`:

  confusion matrix

- `pos`:

  positive class (binary only, else `NULL`)

- `diag`:

  number of correct predictions

- `n`:

  total number of observations

- `acc`, `accLci`, `accUci`:

  accuracy and CI

- `conf.level`:

  confidence level used for the accuracy CI

- `nir`:

  no-information rate

- `accPValue`:

  p-value for accuracy greater than the no-information rate

- `kappa`:

  Cohen's kappa

- `mcnemarPValue`:

  McNemar test p-value

- `byclass`:

  matrix of class-wise metrics

`sensitivity()` and `specificity()` return a named numeric vector
containing the sensitivity or specificity, respectively, for each
reported class.

## Details

This is a generic function with methods for tables, vectors, and several
model objects (e.g., `glm`, `rpart`, `randomForest`, `svm`).

`sensitivity()` and `specificity()` are convenience extractors for the
sensitivity and specificity values computed by `confusion()`.

The orientation of the table matters: rows are read as predictions and
columns as references, so the no-information rate is taken from the
column margin. `confusion.default()` builds the table accordingly.

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
confusion(pred, ref)
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
#> Error in strPad(paste0(rownames(x$byclass), " :"), width = 25L, align = "right"): unused argument (align = "right")

# table
confusion(table(pred, ref))
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
#> Error in strPad(paste0(rownames(x$byclass), " :"), width = 25L, align = "right"): unused argument (align = "right")

# glm
m <- glm(am ~ hp + wt, data = mtcars, family = binomial)
confusion(m)
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
#> Error in strPad(paste0(rownames(x$byclass), " :"), width = 25L, align = "right"): unused argument (align = "right")
```
