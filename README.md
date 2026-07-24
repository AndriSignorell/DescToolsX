
<!-- README.md is generated from README.Rmd. Please edit that file -->


# 📊 DescToolsX

**Version:** 0.0.0.912\
**Title:** Tools for Descriptive Statistics -- New Generation\
**License:** GPL (≥ 2)

<!-- badges: start -->

[![R-CMD-check](https://github.com/AndriSignorell/DescToolsX/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AndriSignorell/DescToolsX/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Tools for Descriptive Statistics and Exploratory Data Analysis

[DescTools](https://cran.r-project.org/web/packages/DescTools/) has been
available on CRAN for more than 12 years and has undergone a large
number of changes and additions during that time. In this timespan the
package gained impressive popularity and was downloaded a million times
in 2025. However its historical development has led to inconsistencies
that could no longer be resolved through an evolutionary process. It was
time to redesign the package to establish a clean new foundation.

![](man/figures/timeToMove.png)

**DescToolsX** is the successor to DescTools, completely redesigned,
decluttered, simplified, bugfixed, unified and substantially
accelerated.

The DescTools collection of functions has been reviewed, reorganised and
grouped into logical units, with particular attention paid to
consistency in operation and user interface design. The new approach
moves away from the monolithic design of DescTools, which had recently
made maintenance so difficult. The functions are now distributed across
several packages, which are, however, loaded directly alongside the main
package, so the user does not need to do anything further.

## 📦 The DescToolsX ecosystem consists of:

**DescToolsX** is the front-end package that automatically loads:

- 🪨 **[bedrock](https://github.com/AndriSignorell/bedrock/)**  
  → Core utility functions used across all packages

- 🌌 **[pharos](https://github.com/AndriSignorell/pharos/)**  
  → Plotting, colour handling, and formatting tools

- 💡 **[lumen](https://github.com/AndriSignorell/lumen/)**  
  → Inferential statistics (tests, confidence intervals, distributions)

- 📨 **[pons](https://github.com/AndriSignorell/pons/)**  
  → MS Office interface and reporting tools


## 🧩 Overview

`DescToolsX` provides a modern, redesigned framework for:

-   descriptive statistics
-   effect sizes
-   agreement measures
-   association statistics
-   transformations
-   model diagnostics
-   inequality metrics
-   epidemiological utilities
-   robust statistics
-   date/time utilities

The package emphasizes:

-   improved consistency
-   cleaner APIs
-   better performance
-   maintainability
-   modern R infrastructure

It depends on the companion ecosystem packages:

-   `pharos`
-   `lumen`
-   `bedrock`
-   `pons`

fileciteturn4file0

------------------------------------------------------------------------

## ⚙️ Installation

``` r
remotes::install_github("AndriSignorell/DescToolsX")
```

------------------------------------------------------------------------

# 📚 Main Function Categories

------------------------------------------------------------------------

## 📈 Descriptive Statistics

Core descriptive statistics utilities:

-   `desc()`
-   `abstract()`
-   `freq()`
-   `freq2D()`
-   `percTable()`
-   `quantileX()`
-   `meanX()`
-   `medianX()`
-   `modeX()`
-   `varX()`
-   `skew()`
-   `kurt()`

Example:

``` r
desc(mtcars)
abstract(mtcars)
```

------------------------------------------------------------------------

## 📊 Effect Sizes

Includes classical effect size statistics:

-   `cohenD()`
-   `glassDelta()`
-   `etaSq()`

Example:

``` r
cohenD(x, y, conf.level = 0.95)
```

------------------------------------------------------------------------

## 🤝 Agreement & Reliability

Advanced inter-rater and reliability statistics:

-   `cohenKappa()`
-   `ccc()` -- concordance correlation coefficient
-   `cronbachAlpha()`
-   `icc()`
-   `kendallW()`
-   `krippAlpha()`
-   `randolphKappa()`

Useful for:

-   clinical agreement studies
-   reliability analysis
-   psychometrics
-   medical validation studies

------------------------------------------------------------------------

## 🔗 Association Measures

Nominal and ordinal association metrics:

-   `cramerV()`
-   `phi()`
-   `lambda()`
-   `somersDelta()`
-   `gkGamma()`
-   `kendallTauA()`
-   `kendallTauB()`
-   `stuartTauC()`
-   `tschuprowT()`
-   `uncertCoef()`
-   `mutInf()`

Wrapper function:

``` r
assocs(table_data)
```

------------------------------------------------------------------------

## 📉 Inequality & Diversity Metrics

Includes metrics from economics and ecology:

-   `gini()`
-   `atkinson()`
-   `theil()`
-   `simpson()`
-   `entropy()`
-   `herfindahl()`
-   `rosenbluth()`

Example:

``` r
gini(income)
atkinson(income, epsilon = 0.5)
```

------------------------------------------------------------------------

## 🔄 Transformations

Data transformation tools:

-   `boxCox()`
-   `boxCoxLambda()`
-   `yeoJohnson()`
-   `scaleX()`
-   `logSt()`

Example:

``` r
lambda <- boxCoxLambda(x)
x_bc <- boxCox(x, lambda)
```

------------------------------------------------------------------------

## 🧪 Model Diagnostics & Performance

Regression and predictive diagnostics:

-   `auc()`
-   `brierScore()`
-   `pseudoR2()`
-   `vif()`

Error metrics:

-   `mae()`
-   `mse()`
-   `rmse()`
-   `mape()`
-   `smape()`

------------------------------------------------------------------------

## 📅 Date & Time Utilities

Convenient helpers:

-   `addMonths()`
-   `as_ym()`
-   `countWorkDays()`
-   `cutAge()`
-   `generation()`
-   `zodiac()`

Example:

``` r
addMonths("2025-01-31", 1)
```

------------------------------------------------------------------------

## 🧠 Robust Statistics

Robust estimators and resistant methods:

-   `huberM()`
-   `tukeyBiweight()`
-   `hodgesLehmann()`
-   `madX()`
-   `meanAD()`

------------------------------------------------------------------------

## 🧰 Utility Functions

Additional utilities include:

-   imputation tools (`impute()`, `imputeKnn()`)
-   confusion matrix utilities
-   correlation tools
-   contingency analysis
-   weighted statistics
-   scaling helpers
-   date conversions

------------------------------------------------------------------------

# 🚀 Design Philosophy

DescToolsX was designed to modernize the original DescTools package
while preserving:

-   statistical breadth
-   practical workflows
-   lightweight usage
-   compatibility with base R

Key improvements include:

-   consistent naming conventions
-   modular architecture
-   improved documentation
-   vectorized implementations
-   cleaner confidence interval handling
-   modern package ecosystem integration

------------------------------------------------------------------------

# 📦 Dependencies

Core dependencies include:

-   `Rcpp`
-   `RcppParallel`
-   `RcppArmadillo`
-   `stats`
-   `boot`
-   `cli`
-   `stringi`

Companion ecosystem:

-   `pharos`
-   `bedrock`
-   `lumen`
-   `pons`

------------------------------------------------------------------------

# 🧪 Example Workflow

``` r
library(DescToolsX)

# descriptive statistics
desc(iris)

# effect size
cohenD(
  iris$Sepal.Length[iris$Species == "setosa"],
  iris$Sepal.Length[iris$Species == "virginica"]
)

# agreement
cohenKappa(matrix(c(50,5,4,40), nrow=2))

# inequality
gini(c(1,2,3,10))

# transformations
lambda <- boxCoxLambda(AirPassengers)
```

------------------------------------------------------------------------

# 🌐 Documentation

-   Website:\
    https://andrisignorell.github.io/DescToolsX/

-   GitHub:\
    https://github.com/AndriSignorell/DescToolsX

-   Issues:\
    https://github.com/AndriSignorell/DescToolsX/issues

------------------------------------------------------------------------

# 📖 Notes

`DescToolsX` is not merely a direct port of `DescTools`.\
It is a structural redesign emphasizing:

-   clearer statistical APIs
-   long-term maintainability
-   separation into ecosystem modules
-   improved computational performance
-   better consistency across functions

------------------------------------------------------------------------

# 📜 License

GPL (≥ 2)

------------------------------------------------------------------------

# 📎 Source

Based on package documentation from the uploaded
`DescToolsX_0.0.0.912.pdf`. 

