
#' DescToolsX: Descriptive Statistics and Exploratory Data Analysis
#'
#' DescToolsX is the sequel to DescTools which provides a modern, consistent, 
#' and extensible framework for
#' descriptive statistics, statistical tests, confidence intervals, and
#' exploratory data analysis.
#'
#' The package is a deliberate redesign inspired by DescTools, with a strong
#' emphasis on naming consistency, predictability, and avoidance of name clashes
#' with base R and other packages.
#'
#' Statistical summary functions that would otherwise mask base R functions are
#' suffixed with `X` (e.g. `meanX()`, `sdX()`, `medianX()`).
#' So functions ending in X are extended versions (typically supporting weights 
#' or implementing confidence intervals) provided by DescToolsX and are 
#' designed to coexist with base R functions without masking them.
#' 
#' DescToolsX follows a strict and consistent naming scheme to ensure
#' predictability and to avoid name clashes with base R and other packages.
#' 
#' @docType package
#' 
#' @name DescToolsX
#' @aliases DescToolsX-package
#' 
#' @section Further principles:
#' Following section explain further principles valid throughout **DescToolsX**:
#'  \tabular{ll}{
#'    \verb{  }[Formulas]             \tab Handling formulas \cr
#'    \verb{  }[Association]\verb{  } \tab Association  \cr
#'    \verb{  }[Agreement]\verb{  } \tab Interrater agreement  \cr
#'    \verb{  }[ConfidenceIntervals]\verb{  } \tab Confidence intervals  \cr
#'    \verb{  }[Association]\verb{  } \tab Measures  \cr
#'    \verb{  }[Association]\verb{  } \tab Plots  \cr
#'    }
#'
#' @section Function names:
#' All functions use **lower camelCase**.
#'
#' @section Statistical summary functions:
#' Statistical functions that would otherwise mask base R functions are suffixed
#' with `X`. This explicitly signals an extended or modified implementation.
#'
#' Examples:
#' `meanX()`, `medianX()`, `sdX()`, `madX()`, `iqrX()`,
#' `varX()`, `quantileX()`, `skew()`, `kurt()`
#'
#' @section Confidence interval functions:
#' Functions computing confidence intervals use the suffix `CI`, following
#' established R conventions.
#'
#' Examples:
#' `meanCI()`, `medianCI()`, `sdCI()`, `varCI()`,
#' `quantileCI()`
#'
#' @section Statistical tests:
#' Statistical tests use lower camelCase and end with `Test`.
#'
#' Examples:
#' `shapiroFranciaTest()`, `andersonDarlingTest()`,
#' `leveneTest()`, `jarqueBeraTest()`
#'
#' @section Plot functions:
#' Plotting functions start with the prefix `plot` and use lower camelCase.
#'
#' Examples:
#' `plotQQ()`, `plotECDF()`, `plotCor()`, `plotViolin()`
#'
#' @section Classes and S3 methods:
#' Classes use **UpperCamelCase**. S3 methods follow standard R conventions.
#'
#' Examples:
#' `desc.numeric`, `percTable`, `print.PercTable`,
#' `plot.Desc.numeric`
#'
#' @details
#' Consistency and predictability take precedence over historical base R naming
#' conventions. This design choice is a key difference between DescToolsX and
#' DescTools.
#' 
#' **Design principles**
#'
#' DescToolsX follows a set of strict design principles to ensure consistency,
#' usability, and performance across the entire package.
#'
#' @section Argument order:
#' Function arguments follow a consistent and predictable order:
#' \enumerate{
#'   \item `x` (primary data object)
#'   \item method-specific parameters
#'   \item confidence-related parameters (e.g. `conf.level`)
#'   \item formatting and display options
#'   \item `...` (additional arguments)
#' }
#'
#' This ordering is applied uniformly across statistical summary functions,
#' confidence interval functions, and plotting functions.
#'
#' @section Confidence interval functions:
#' Confidence interval functions follow the same argument order as their
#' corresponding point estimators. In particular, `conf.level` is always
#' used to specify the confidence level and appears explicitly as a named
#' argument.
#'
#' Examples:
#' `meanCI(x, conf.level = 0.95)`,
#' `medianCI(x, conf.level = 0.95)`
#'
#' @section Plot functions:
#' Plotting functions follow the same data-first argument convention. The data
#' object is always the first argument, followed by plot-specific parameters and
#' graphical options. This ensures intuitive usage and consistent behaviour
#' across different plot types.
#'
#' Examples:
#' `plotQQ(x)`, `plotECDF(x)`, `plotCor(x, method = "spearman")`
#'
#' @section Performance and implementation:
#' Computationally intensive functionality is systematically reimplemented
#' using \pkg{Rcpp}. This replaces former pure R implementations and results in
#' substantially improved runtime performance while preserving numerical
#' accuracy and user-facing behaviour.
#'
#' Performance improvements are a core design goal of DescToolsX and a key
#' motivation for the package redesign.
#' 
#' pharos is listed in Depends because this package extends its
#' user-facing API and expects it to be attached. Functions used internally
#' are explicitly imported via the NAMESPACE.
#' 
"_PACKAGE"

