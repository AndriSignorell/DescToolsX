# 📦 DescToolsX

**Title:** Tools for Descriptive Statistics — New Generation  
**License:** GPL (≥ 2)

## 🧩 Overview

`DescToolsX` is the descriptive-statistics layer of the **DescToolsX
ecosystem** and the redesigned successor to `DescTools`. It collects the
routines needed to describe, summarise and explore data before any model
is fitted: frequency and contingency tables, measures of location,
dispersion, shape and concentration, association and agreement
coefficients, effect sizes, and metrics for evaluating classifiers.

A single [`desc()`](reference/Desc.md) generic dispatches on the type of
the input, so a numeric vector, a factor, a pair of variables or a whole
data frame are all described through the same entry point.

📖 **Documentation:** <https://andrisignorell.github.io/DescToolsX/>

## ⚙️ Installation

``` r

install.packages("DescToolsX")
```

Or the development version from GitHub:

``` r

remotes::install_github("AndriSignorell/DescToolsX")
```

## 📚 Core Features

### 🔹 Describing Data

- [`desc()`](reference/Desc.md) — one generic, dispatching on vector,
  factor, date, numeric pair, table or data frame
- [`abstract()`](reference/abstract.md) — compact structure of a data
  frame including labels
- [`freq()`](reference/freq.md), [`freq2D()`](reference/freq2D.md),
  [`percTable()`](reference/percTable.md),
  [`expFreq()`](reference/expFreq.md) — frequency and contingency tables
- [`tOne()`](reference/tOne.md) — “table one” style summaries

### 🔹 Location, Dispersion and Shape

- [`meanX()`](reference/meanX.md), [`medianX()`](reference/medianX.md),
  [`modeX()`](reference/modeX.md),
  [`quantileX()`](reference/quantileX.md),
  [`rangeX()`](reference/rangeX.md)
- [`gmean()`](reference/gmean.md), [`hmean()`](reference/hmean.md),
  [`huberM()`](reference/huberM.md),
  [`tukeyBiweight()`](reference/tukeyBiweight.md),
  [`hodgesLehmann()`](reference/hodgesLehmann.md)
- [`varX()`](reference/varX.md), [`madX()`](reference/madX.md),
  [`iqrX()`](reference/iqrX.md), [`meanAD()`](reference/meanAD.md),
  [`meanSE()`](reference/meanSE.md), [`coefVar()`](reference/coefVar.md)
- [`skew()`](reference/skew.md), [`kurt()`](reference/kurt.md)

### 🔹 Association and Correlation

- Nominal: [`cramerV()`](reference/cramerV.md),
  [`contCoef()`](reference/contCoef.md), [`phi()`](reference/phi.md),
  [`tschuprowT()`](reference/tschuprowT.md),
  [`lambda()`](reference/lambda.md),
  [`uncertCoef()`](reference/uncertCoef.md),
  [`gkTau()`](reference/gkTau.md), [`yule()`](reference/yule.md)
- Ordinal: [`ordAssocs()`](reference/ordAssocs.md),
  [`conDisPairs()`](reference/conDisPairs.md),
  [`kendallW()`](reference/kendallW.md)
- Continuous: [`pearsonCor()`](reference/pearsonCor.md),
  [`spearmanCor()`](reference/spearmanCor.md),
  [`corPart()`](reference/corPart.md),
  [`corPolychor()`](reference/corPolychor.md),
  [`hoeffdingD()`](reference/hoeffdingD.md),
  [`findCorrX()`](reference/findCorrX.md)
- [`Association()`](reference/Association.md) — common interface across
  the measures

### 🔹 Agreement and Reliability

- [`cohenKappa()`](reference/cohenKappa.md),
  [`kappaM()`](reference/kappaM.md),
  [`randolphKappa()`](reference/randolphKappa.md),
  [`krippAlpha()`](reference/krippAlpha.md)
- [`icc()`](reference/icc.md), [`ccc()`](reference/ccc.md),
  [`percAgreement()`](reference/percAgreement.md),
  [`pabak()`](reference/pabak.md),
  [`raterFrame()`](reference/raterFrame.md)
- [`cronbachAlpha()`](reference/cronbachAlpha.md),
  [`blandAltmanData()`](reference/blandAltmanData.md)
- [`Agreement()`](reference/Agreement.md) — common interface across the
  measures

### 🔹 Effect Sizes

- [`cohenD()`](reference/cohenD.md), [`cohenH()`](reference/cohenH.md),
  [`glassDelta()`](reference/glassDelta.md),
  [`etaSq()`](reference/etaSq.md)

### 🔹 Model and Classifier Metrics

- [`auc()`](reference/auc.md), [`cStat()`](reference/cStat.md),
  [`brierScore()`](reference/brierScore.md)
- [`mae()`](reference/mae.md), [`mape()`](reference/mape.md),
  [`mse()`](reference/mse.md), [`rmse()`](reference/rmse.md),
  [`nmae()`](reference/nmae.md), [`nmse()`](reference/nmse.md),
  [`smape()`](reference/smape.md)
- [`isConfusionTable()`](reference/isConfusionTable.md),
  [`normalizeToConfusion()`](reference/normalizeToConfusion.md)
- [`oddsRatio()`](reference/oddsRatio.md),
  [`relRisk()`](reference/relRisk.md)

### 🔹 Inequality and Diversity

- [`gini()`](reference/gini.md), [`atkinson()`](reference/atkinson.md),
  [`theil()`](reference/theil.md), [`lc()`](reference/Lc.md) (Lorenz
  curve)
- [`herfindahl()`](reference/herfindahl.md),
  [`rosenbluth()`](reference/rosenbluth.md),
  [`simpson()`](reference/simpson.md),
  [`divCoef()`](reference/divCoef.md)
- [`entropy()`](reference/entropy.md), [`mutInf()`](reference/mutInf.md)

### 🔹 Dates and Time

- [`addMonths()`](reference/AddMonths.md),
  [`as_ym()`](reference/as_ym.md),
  [`countWorkDays()`](reference/countWorkDays.md),
  [`generation()`](reference/generation.md),
  [`zodiac()`](reference/zodiac.md)
- date predicates and conversions, [`cutAge()`](reference/cutAge.md)

### 🔹 Transformation, Binning and Missing Values

- [`boxCox()`](reference/boxCox.md),
  [`boxCoxLambda()`](reference/boxCoxLambda.md),
  [`yeoJohnson()`](reference/yeoJohnson.md),
  [`logSt()`](reference/logSt.md), [`scaleX()`](reference/scaleX.md)
- [`cutQ()`](reference/cutQ.md),
  [`cut.integer()`](reference/cut.integer.md)
- [`impute()`](reference/impute.md),
  [`imputeKnn()`](reference/imputeKnn.md)
- [`outlier()`](reference/outlier.md),
  [`extremes()`](reference/extremes.md), [`lof()`](reference/lof.md)

## 🚀 Design Principles

- **Consistent** — lowerCamelCase API and uniform argument conventions
  across the whole DescToolsX suite
- **Fast** — performance-critical routines implemented in Rcpp and
  RcppArmadillo
- **Generic** — S3 generics with methods for vectors, factors, matrices,
  tables, and data frames
- **Robust** — validated inputs, informative errors, extensive testthat
  coverage

## 🧪 Example

``` r

library(DescToolsX)

# one generic for very different inputs
desc(iris$Sepal.Length)
desc(iris$Species)
desc(iris)

# frequency table with cumulative columns
freq(iris$Species)

# association between two factors
cramerV(table(mtcars$cyl, mtcars$gear))

# effect size and confidence interval
cohenD(mpg ~ am, data = mtcars)
```

## 🧱 The Suite

`DescToolsX` builds on `bedrock` (base utilities), `pharos` (graphics)
and `lumen` (tests, confidence intervals, distributions). `alloy`
(modelling), `pons` (MS-Office) and `swissValet` (RStudio addins)
complete the family.

## 🙏 Acknowledgements

Parts of the code and documentation were reviewed with the help of large
language models (OpenAI Codex, Anthropic Claude). Every suggestion was
assessed, edited and verified by the maintainer, who remains solely
responsible for the content of this package.

## 📜 License

GPL (≥ 2)
