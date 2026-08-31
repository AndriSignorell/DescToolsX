# Package index

## Package Overview & Interfaces

Package overview, shared interfaces, constants, options, and
documentation tools.

- [`DescToolsX`](DescToolsX.md) [`DescToolsX-package`](DescToolsX.md) :
  DescToolsX: Descriptive Statistics and Exploratory Data Analysis
- [`attachAliases()`](attach-detach-aliases.md)
  [`detachAliases()`](attach-detach-aliases.md) : Attach and Remove
  Short Aliases for Selected DescToolsX Functions
- [`Agreement`](Agreement.md) : Agreement Measures - Common Interface
- [`Association`](Association.md) : Association Measures - Common
  Interface
- [`ConfidenceIntervals`](ConfidenceIntervals.md) : Confidence Interval
  Interface - Common Arguments
- [`Formulas`](Formulas.md) : Formula Interfaces - Common Arguments
- [`getConcepts()`](concepts.md) [`conceptMap()`](concepts.md)
  [`conceptAudit()`](concepts.md) : Concept Utilities for Package
  Documentation
- [`day.abb`](constants.md) [`day.name`](constants.md) : DescToolsX
  Constants
- [`setDescToolsXOption()`](setDescToolsXOption.md) : Set DescToolsX
  Options

## Describing Data & Tables

Compact inspection, descriptive summaries, and one- and two-dimensional
tables.

- [`abstract()`](abstract.md) [`print(`*`<Abstract>`*`)`](abstract.md) :
  Display Compact Abstract of a Data Frame

- [`desc()`](Desc.md) [`print(`*`<Desc.list>`*`)`](Desc.md)
  [`print(`*`<Desc>`*`)`](Desc.md) [`plot(`*`<Desc>`*`)`](Desc.md)
  [`print(`*`<Desc.AllNA>`*`)`](Desc.md)
  [`plot(`*`<Desc.AllNA>`*`)`](Desc.md)
  [`plot(`*`<Desc.factor>`*`)`](Desc.md)
  [`print(`*`<Desc.logical>`*`)`](Desc.md)
  [`plot(`*`<Desc.logical>`*`)`](Desc.md)
  [`print(`*`<Desc.numeric>`*`)`](Desc.md)
  [`plot(`*`<Desc.numeric>`*`)`](Desc.md) : Describe Data

- [`desc(`*`<Date>`*`)`](Desc.Date.md) : Descriptive statistics for
  calendar date variables

- [`desc(`*`<factor>`*`)`](Desc.factor.md)
  [`desc(`*`<character>`*`)`](Desc.factor.md)
  [`print(`*`<Desc.factor>`*`)`](Desc.factor.md) : Describe a Factor

- [`.descNN()`](Desc.nn.md) [`print(`*`<Desc.nn>`*`)`](Desc.nn.md)
  [`plot(`*`<Desc.nn>`*`)`](Desc.nn.md) : Describe a Numeric-Numeric
  Relationship

- [`.descNQ()`](desc.nq.md) [`print(`*`<Desc.nq>`*`)`](desc.nq.md)
  [`plot(`*`<Desc.nq>`*`)`](desc.nq.md) : Describe Relationship: Numeric
  x by Categorical g

- [`desc(`*`<numeric>`*`)`](desc.numeric.md) : Describe a Numeric
  Variable

- [`.descQN()`](desc.qn.md) [`print(`*`<Desc.qn>`*`)`](desc.qn.md) :
  Describe Relationship: Categorical y vs Numeric x

- [`.descQQ()`](desc.qq.md) : Describe Relationship: Categorical x by
  Categorical y

- [`desc(`*`<ts>`*`)`](desc.ts.md)
  [`print(`*`<Desc.ts>`*`)`](desc.ts.md)
  [`plot(`*`<Desc.ts>`*`)`](desc.ts.md) : Diagnostic Summary for Time
  Series Objects

- [`print(`*`<Desc.Date>`*`)`](print.Desc.Date.md) :

  Print method for `"Desc.Date"` objects

- [`print(`*`<Desc.qq>`*`)`](desc.table.md)
  [`plot(`*`<Desc.qq>`*`)`](desc.table.md)
  [`desc(`*`<table>`*`)`](desc.table.md)
  [`desc(`*`<matrix>`*`)`](desc.table.md)
  [`desc(`*`<array>`*`)`](desc.table.md)
  [`print(`*`<Desc.table>`*`)`](desc.table.md) : Describe a Contingency
  Table

- [`expFreq()`](expFreq.md) : Expected Frequencies

- [`freq()`](freq.md) [`print(`*`<Freq>`*`)`](freq.md) : Frequency Table
  for a Single Variable

- [`freq2D()`](freq2D.md) : Bivariate (Two-Dimensional) Frequency
  Distribution

- [`percTable()`](percTable.md)
  [`print(`*`<PercTable>`*`)`](percTable.md) : Percentage Table

- [`tOne()`](tOne.md) [`print(`*`<tOne>`*`)`](tOne.md)
  [`` `[`( ``*`<tOne>`*`)`](tOne.md) : Create Table One Describing
  Baseline Characteristics

## Location, Dispersion & Shape

Classical, weighted, robust, and nonlinear descriptive statistics.

- [`gmean()`](gmean.md) [`gsd()`](gmean.md) : Geometric Mean and
  Standard Deviation
- [`hmean()`](hmean.md) : Harmonic Mean and Its Confidence Interval
- [`hodgesLehmann()`](hodgesLehmann.md) : Hodges-Lehmann Estimator of
  Location
- [`huberM()`](huberM.md) : Safe (Generalized) Huber M-Estimator of
  Location
- [`meanX()`](meanX.md) : (Weighted) Arithmetic Mean
- [`medianX()`](medianX.md) : (Weighted) Median Value
- [`modeX()`](modeX.md) : Mode (most Frequent Value(s))
- [`tukeyBiweight()`](tukeyBiweight.md) : Tukey's Biweight Mean
- [`coefVar()`](coefVar.md) [`coefVarCI()`](coefVar.md) : Coefficient of
  Variation
- [`iqrX()`](iqrX.md) : The (weighted) Interquartile Range
- [`madX()`](madX.md) : Median Absolute Deviation
- [`meanAD()`](meanAD.md) : Mean Absolute Deviation From a Center Point
- [`meanSE()`](meanSE.md) : Standard Error of Mean
- [`rangeX()`](rangeX.md) : (Robust) Range
- [`sdX()`](varX.md) [`varX()`](varX.md) : (Weighted) Variance and
  Standard Deviation
- [`large()`](extremes.md) [`small()`](extremes.md)
  [`highLow()`](extremes.md) : Kth Smallest/Largest Values
- [`kurt()`](kurt.md) : Kurtosis
- [`quantileX()`](quantileX.md) : (Weighted) Sample Quantiles
- [`skew()`](skew.md) : Skewness

## Association & Correlation

Nominal, ordinal, and continuous association measures and supporting
tools.

- [`contCoef()`](contCoef.md) : Pearson's Contingency Coefficient
- [`cramerV()`](cramerV.md) : Cramer's V
- [`gkTau()`](gkTau.md) : Goodman Kruskal's Tau
- [`lambda()`](lambda.md) : Goodman Kruskal Lambda
- [`mutInf()`](mutInf.md) : Mutual Information
- [`phi()`](phi.md) : Phi Coefficient
- [`tschuprowT()`](tschuprowT.md) : Tschuprow's T
- [`uncertCoef()`](uncertCoef.md) : Uncertainty Coefficient
- [`yuleQ()`](yule.md) [`yuleY()`](yule.md) : Yule's Coefficients of
  Association (Q and Y)
- [`conDisPairs()`](conDisPairs.md) : Concordant and Discordant Pairs
- [`kendallW()`](kendallW.md) : Kendall's Coefficient of Concordance W
- [`ordAssocs()`](ordAssocs.md) [`gkGamma()`](ordAssocs.md)
  [`kendallTauA()`](ordAssocs.md) [`kendallTauB()`](ordAssocs.md)
  [`stuartTauC()`](ordAssocs.md) [`somersDelta()`](ordAssocs.md) :
  Ordinal Association Measures
- [`corPart()`](corPart.md) : Partial Correlation Matrix
- [`corPolychor()`](corPolychor.md) : Polychoric Correlation
- [`findCorrX()`](findCorrX.md) : Identify Highly Correlated Variables
- [`hoeffdingD()`](hoeffdingD.md) : Hoeffding's D Statistic
- [`keepSig()`](keepSig.md) : Keep Only Significant Values in a
  Symmetric Matrix
- [`pearsonCor()`](pearsonCor.md) : Confidence Intervals for Pearson
  Correlation
- [`spearmanCor()`](spearmanCor.md) : Spearman Rank Correlation

## Agreement & Reliability

Agreement measures, reliability coefficients, rater data, and supporting
tools.

- [`blandAltmanData()`](blandAltmanData.md) : Bland-Altman Agreement
  Data
- [`ccc()`](ccc.md) : Lin's Concordance Correlation Coefficient
- [`print(`*`<BlandAltman>`*`)`](print.BlandAltman.md) : Print a
  Bland-Altman Analysis
- [`cohenKappa()`](cohenKappa.md) : Cohen's Kappa and Weighted Kappa
- [`kappaM()`](kappaM.md) : Kappa for m Raters
- [`krippAlpha()`](krippAlpha.md) : Krippendorff's Alpha for Wide Data
- [`pabak()`](pabak.md) : Prevalence-Adjusted and Bias-Adjusted Kappa
  (PABAK)
- [`percAgreement()`](percAgreement.md) : Percent Agreement with
  Design-Based SE and CI
- [`randolphKappa()`](randolphKappa.md) : Randolph's Free-Marginal
  Multirater Kappa
- [`cronbachAlpha()`](cronbachAlpha.md) : Cronbach's Coefficient Alpha
- [`icc()`](icc.md) : Intraclass Correlation Coefficient (ICC)
- [`isConfusionTable()`](isConfusionTable.md) : Detect Whether an Object
  Looks Like a Confusion/Coincidence Matrix
- [`normalizeToConfusion()`](normalizeToConfusion.md) : Normalize Input
  to a Contingency or Agreement Table
- [`raterFrame()`](raterFrame.md) : Create a Data.frame for Interrater
  Agreement

## Effect Sizes & Binary Outcomes

Standardized effects, ANOVA effects, odds ratios, and relative risks.

- [`cohenD()`](cohenD.md) : Cohen's and Hedges' Effect Size
- [`etaSq()`](etaSq.md) [`aovlDetails()`](etaSq.md)
  [`aovlErrorTerms()`](etaSq.md) : Effect Size Calculations for ANOVAs
- [`glassDelta()`](glassDelta.md) : Glass' Delta Effect Size
- [`cohenH()`](cohenH.md) : Cohen's h for a 2x2 Table
- [`oddsRatio()`](oddsRatio.md)
  [`print(`*`<OddsRatio>`*`)`](oddsRatio.md) : Compute Odds Ratios
- [`relRisk()`](relRisk.md) : Relative Risk

## Inequality, Diversity & Concentration

Inequality curves and indices, diversity, entropy, and concentration
measures.

- [`atkinson()`](atkinson.md) : Atkinson Index
- [`gini()`](gini.md) : Gini Coefficient
- [`herfindahl()`](herfindahl.md) : Herfindahl Index
- [`lc(`*`<formula>`*`)`](Lc.md) [`lc(`*`<default>`*`)`](Lc.md)
  [`predict(`*`<Lc>`*`)`](Lc.md) : Lorenz Curve
- [`rosenbluth()`](rosenbluth.md) : Rosenbluth Index
- [`theil()`](theil.md) : Theil Index
- [`divCoef()`](divCoef.md) : Compute a diversity coefficient
- [`entropy()`](entropy.md) : Shannon Entropy
- [`simpson()`](simpson.md) : Simpson Diversity Indices

## Classification & Model Evaluation

Classification metrics, calibration, discrimination, and
prediction-error measures.

- [`auc()`](auc.md) : Compute Area Under the Curve
- [`conf()`](conf.md) [`print(`*`<Conf>`*`)`](conf.md)
  [`plot(`*`<Conf>`*`)`](conf.md) [`sensX()`](conf.md)
  [`specX()`](conf.md) : Confusion Matrix and Classification Metrics
- [`cStat()`](cStat.md) : Concordance Statistic (C-Statistic / AUC)
- [`brierScore()`](brierScore.md) : Brier Score
- [`mae()`](mae.md) : Mean Absolute Error
- [`mape()`](mape.md) : Mean Absolute Percentage Error
- [`mse()`](mse.md) : Mean Squared Error
- [`nmae()`](nmae.md) : Normalized Mean Absolute Error
- [`nmse()`](nmse.md) : Normalized Mean Squared Error
- [`rmse()`](rmse.md) : Root Mean Squared Error
- [`smape()`](smape.md) : Symmetric Mean Absolute Percentage Error

## Transformations, Missing Data & Outliers

Binning, transformations, imputation, scaling, and outlier detection.

- [`cut(`*`<integer>`*`)`](cut.integer.md) : Cut an Integer Variable
  into Intervals
- [`cutAge()`](cutAge.md) : Create a Factor Variable by Cutting an Age
  Variable
- [`cutQ()`](cutQ.md) : Create a Factor Variable Using the Quantiles of
  a Continuous Variable
- [`boxCox()`](boxCox.md) [`boxCoxInv()`](boxCox.md) : Box-Cox
  Transformation
- [`boxCoxLambda()`](boxCoxLambda.md) : Automatic Selection of Box-Cox
  Transformation Parameter
- [`logSt()`](logSt.md) [`logStInv()`](logSt.md) : Started Logarithmic
  Transformation and Its Inverse
- [`scaleX()`](scaleX.md) : (Robust) Scaling and Centering
- [`yeoJohnson()`](yeoJohnson.md) [`yeoJohnsonInv()`](yeoJohnson.md) :
  Yeo-Johnson Transformation
- [`impute()`](impute.md) : Impute Missing Values in a Vector
- [`imputeKnn()`](imputeKnn.md) : K-Nearest Neighbors Imputation
- [`lof()`](lof.md) : Local Outlier Factor
- [`outlier()`](outlier.md) : Outlier

## Dates & Time

Date classes, calendar arithmetic, extraction, conversion, and
categorization.

- [`as.ym()`](as_ym.md) [`as.Date(`*`<ym>`*`)`](as_ym.md)
  [`print(`*`<ym>`*`)`](as_ym.md) : A Class for Dealing with the
  Yearmonth Format
- [`isDate()`](date-time-predicates.md)
  [`isTime()`](date-time-predicates.md)
  [`isDateTime()`](date-time-predicates.md)
  [`hasVaryingTime()`](date-time-predicates.md) : Date and Time Class
  Predicates
- [`addMonths()`](AddMonths.md) : Add Months to a Date
- [`countWorkDays()`](countWorkDays.md) : Count Work Days Between Two
  Dates
- [`year()`](date_functions.md) [`month()`](date_functions.md)
  [`week()`](date_functions.md) [`day()`](date_functions.md)
  [`` `day<-`() ``](date_functions.md) [`weekday()`](date_functions.md)
  [`quarter()`](date_functions.md) [`today()`](date_functions.md)
  [`now()`](date_functions.md) [`hour()`](date_functions.md)
  [`minute()`](date_functions.md) [`second()`](date_functions.md)
  [`timezone()`](date_functions.md) [`yearMonth()`](date_functions.md)
  [`yearWeek()`](date_functions.md) [`yearDay()`](date_functions.md)
  [`diffDays360()`](date_functions.md)
  [`lastDayOfMonth()`](date_functions.md)
  [`yearDays()`](date_functions.md) [`monthDays()`](date_functions.md)
  [`isWeekend()`](date_functions.md) [`isLeapYear()`](date_functions.md)
  : Basic Date Functions
- [`hmsToMinute()`](time-conversions.md)
  [`hmsToSec()`](time-conversions.md)
  [`secToHms()`](time-conversions.md) : Convert h:m:s To/From seconds
- [`generation()`](generation.md) : Generation by Birth Year
- [`zodiac()`](zodiac.md) : Calculate the Zodiac of a Date
