#' Association Measures - Common Interface
#'
#' All association measures in this package share a common interface. 
#'
#' @name Association
#' 
#' @details
#' The association is defined between two variables that can be 
#' provided to the calculation functions in different ways. 
#' If only `x` is passed, this can either be a matrix, which 
#' is then interpreted as a contingency table (this seems in the case of frequency
#' data the natural interpretation and is by the way also what
#' [chisq.test()] expects). However, it can also be a data.frame or  
#' a list, which must then contain exactly 2 elements containing the data. 
#' Alternatively, two single data vectors `x` and `y` can be passed. 
#' The two element arguments are typically processed by forming a contingency 
#' table with `[table](x, y, ...)`. `NAs` are by default handled the same 
#' way as the function does, so `NAs` omitted. 
#' 
#' If the measure should be calculated pairwise for a set of variables 
#' [bedrock::pairApply()] can be used. This easily allows to create matrices 
#' of association measures (the same way as the `cor` does). `NAs` 
#' again are by default omitted pairwise, which corresponds to the 
#' `pairwise.complete` option of [cor()]. 
#' Use [complete.cases()], if only the complete
#' cases of a `data.frame` are to be used. (see examples)
#' 
#' Most functions support calculation of confidence intervals.
#' These can be requested by setting `conf.level` to the desired value (usually 0.95). 
#' If it is set to `NA`, no confidence interval is computed. 
#' One-sided confidence intervals
#' can be controlled using the `sides` argument. It names the side on
#' which the *finite* bound lies, which is NOT the convention used for
#' the alternative hypothesis of a test: `"left"` yields an interval
#' bounded below and corresponds to an alternative of `"greater"`.
#' See [ConfidenceIntervals]. Frequently there is a classic and 
#' a bootstrap approach (`"classic"`, `"boot"`). 
#' However most measures have their own specific confidence intervals methods.
#' 
#' Some association measures define additional parameters such as `direction`,
#' `base`, or `correct`. Those are documented with the respective
#' functions.
#' 
#' 
#' **Function List**
#' 
#' Following association measures are implemented in **DescToolsX**:
#'  \tabular{ll}{
#'    \verb{  }[cramerV]          \tab Cramer's V \cr
#'    \verb{  }[contCoef]         \tab Pearson's Contingency Coefficient \cr
#'    \verb{  }[lambda]           \tab Goodman's Lambda \cr
#'    \verb{  }[gkTau]            \tab Goodman Kruskal's Tau \cr
#'    \verb{  }[gkGamma]          \tab Goodman Kruskal's Gamma \cr
#'    \verb{  }[kendallTauB]      \tab Kendall's Tau-b \cr
#'    \verb{  }[stuartTauC]       \tab Stuart's Tau-c \cr
#'    \verb{  }[somersDelta]      \tab Somers' Delta \cr
#'    \verb{  }[uncertCoef]       \tab Theil's Uncertainty Coefficient \cr
#'    \verb{  }[mutInf]           \tab Mutual Information \cr
#'    \verb{  }[hoeffdingD]       \tab Hoeffding's D \cr
#'    \verb{  }[corPolychor]      \tab Polychoric Correlation \cr
#'    }
#'
#' @param x either a contingency table, a two-column object
#'   (matrix, data.frame or list), or a vector of observations 
#'   (together with `y`)
#' @param y optional second vector. If `x` is not a vector,
#'   `y` must be `NULL`.
#'   
#'
#' @references 
#' Cramer, H. (1946) *Mathematical Methods of Statistics*. Princeton
#' University Press
#' 
#' Agresti, Alan (1996) *Introduction to categorical data analysis*. NY:
#' John Wiley and Sons
#' 
#' @concept association-measure
NULL
