# Concept Utilities for Package Documentation

Helper functions to inspect and analyse the use of `\\concept` tags
within a package.

## Usage

``` r
getConcepts(pkg)

conceptMap(pkg)

conceptAudit(pkg)
```

## Arguments

- pkg:

  character string. Name of the installed package.

## Value

- `getConcepts`:

  character vector of unique concept names, sorted

- `conceptMap`:

  named list mapping concepts to topics

- `conceptAudit`:

  data frame with the columns `concept` and `nTopics`, ordered by
  decreasing frequency

## Details

These utilities extract concept metadata from Rd files and allow
structured auditing of conceptual organisation inside a package.

**Functions**

- `getConcepts()` - Returns all unique concepts used in a package.

- `conceptMap()` - Returns a mapping of concepts to functions.

- `conceptAudit()` - Returns a summary table of concept usage.

The functions use [`Rd_db`](https://rdrr.io/r/tools/Rdutils.html) to
parse Rd files and extract `\\concept` tags programmatically.

These tools are intended for package development, documentation
consistency checks, and conceptual audits.

## Examples

``` r
getConcepts("DescToolsX")
#>  [1] "agreement"                                              
#>  [2] "anova-effect-size"                                      
#>  [3] "assoc.agreement"                                        
#>  [4] "assoc.continuous"                                       
#>  [5] "assoc.nominal"                                          
#>  [6] "assoc.ordinal"                                          
#>  [7] "association-measure"                                    
#>  [8] "association-measures"                                   
#>  [9] "binary-association"                                     
#> [10] "binary-outcome"                                         
#> [11] "binning"                                                
#> [12] "bivariate numeric regression correlation scatterplot"   
#> [13] "calibration"                                            
#> [14] "categorical-agreement"                                  
#> [15] "categorization"                                         
#> [16] "chi-square-based"                                       
#> [17] "class-predicate"                                        
#> [18] "classification"                                         
#> [19] "concentration-index"                                    
#> [20] "concordance"                                            
#> [21] "confusion-matrix"                                       
#> [22] "convenience"                                            
#> [23] "correlation"                                            
#> [24] "cut"                                                    
#> [25] "data-description"                                       
#> [26] "data.inspection"                                        
#> [27] "date-handling"                                          
#> [28] "date-time"                                              
#> [29] "date.time"                                              
#> [30] "demographics"                                           
#> [31] "density-based"                                          
#> [32] "desc"                                                   
#> [33] "descriptive"                                            
#> [34] "descriptive-statistics"                                 
#> [35] "dispersion"                                             
#> [36] "distribution-summary"                                   
#> [37] "diversity"                                              
#> [38] "diversity.concentration"                                
#> [39] "effect-size"                                            
#> [40] "effect.size"                                            
#> [41] "factor-handling"                                        
#> [42] "feature-selection"                                      
#> [43] "frequency"                                              
#> [44] "frequency-table"                                        
#> [45] "hypothesis-testing"                                     
#> [46] "imputation"                                             
#> [47] "impute"                                                 
#> [48] "inequality"                                             
#> [49] "information-theory"                                     
#> [50] "internal-consistency"                                   
#> [51] "interrater-agreement"                                   
#> [52] "introspection"                                          
#> [53] "latent-variable"                                        
#> [54] "location"                                               
#> [55] "method-comparison"                                      
#> [56] "missing-value"                                          
#> [57] "model-evaluation"                                       
#> [58] "model.classification"                                   
#> [59] "model.metrics"                                          
#> [60] "moments"                                                
#> [61] "multiple-testing"                                       
#> [62] "nominal"                                                
#> [63] "nonlinear-association"                                  
#> [64] "nonlinear-mean"                                         
#> [65] "order-statistic"                                        
#> [66] "ordinal"                                                
#> [67] "outlier"                                                
#> [68] "outlier-detection"                                      
#> [69] "outlier-resistance"                                     
#> [70] "pearson spearman r-squared residuals heteroscedasticity"
#> [71] "pkg.introspection"                                      
#> [72] "prediction-error"                                       
#> [73] "programming"                                            
#> [74] "quantile"                                               
#> [75] "rank-correlation"                                       
#> [76] "rater-data"                                             
#> [77] "reliability"                                            
#> [78] "robust-statistic"                                       
#> [79] "robust-statistics"                                      
#> [80] "roc"                                                    
#> [81] "shape"                                                  
#> [82] "standardization"                                        
#> [83] "summary"                                                
#> [84] "table-manipulation"                                     
#> [85] "table-summary"                                          
#> [86] "time-series"                                            
#> [87] "transform"                                              
#> [88] "transformation"                                         
#> [89] "utils"                                                  
#> [90] "variance-analysis"                                      
#> [91] "variance-component"                                     
#> [92] "variance-stabilization"                                 
head(conceptMap("DescToolsX"))
#> $agreement
#> [1] "blandAltmanData"      "icc"                  "isConfusionTable"    
#> [4] "krippAlpha"           "normalizeToConfusion" "percAgreement"       
#> [7] "randolphKappa"        "raterFrame"          
#> 
#> $`anova-effect-size`
#> [1] "etaSq"
#> 
#> $assoc.agreement
#> [1] "ccc"           "cohenKappa"    "cronbachAlpha" "icc"          
#> [5] "kappaM"        "krippAlpha"    "pabak"         "percAgreement"
#> [9] "randolphKappa"
#> 
#> $assoc.continuous
#> [1] "corPart"     "corPolychor" "findCorrX"   "hoeffdingD"  "keepSig"    
#> [6] "pearsonCor"  "spearmanCor"
#> 
#> $assoc.nominal
#> [1] "contCoef"   "cramerV"    "gkTau"      "lambda"     "mutInf"    
#> [6] "phi"        "tschuprowT" "uncertCoef" "yule"      
#> 
#> $assoc.ordinal
#> [1] "cStat"       "conDisPairs" "kendallW"    "ordAssocs"  
#> 
head(conceptAudit("DescToolsX"))
#>                   concept nTopics
#> 7     association-measure      17
#> 34 descriptive-statistics      11
#> 32                   desc      10
#> 3         assoc.agreement       9
#> 5           assoc.nominal       9
#> 25       data-description       9
```
