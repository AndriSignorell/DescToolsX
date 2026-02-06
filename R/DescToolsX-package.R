
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
#' suffixed with \code{X} (e.g. \code{meanX()}, \code{sdX()}, \code{medianX()}).
#' So functions ending in X are extended versions provided by DescToolsX and are 
#' designed to coexist with base R functions without masking them.
#' 
#' DescToolsX follows a strict and consistent naming scheme to ensure
#' predictability and to avoid name clashes with base R and other packages.
#' 
#' @name DescToolsX
#'
#' @section Function names:
#' All functions use \strong{lower camelCase}.
#'
#' @section Statistical summary functions:
#' Statistical functions that would otherwise mask base R functions are suffixed
#' with \code{X}. This explicitly signals an extended or modified implementation.
#'
#' Examples:
#' \code{meanX()}, \code{medianX()}, \code{sdX()}, \code{madX()}, \code{iqrX()},
#' \code{varX()}, \code{quantileX()}, \code{skewX()}, \code{kurtX()}
#'
#' @section Confidence interval functions:
#' Functions computing confidence intervals use the suffix \code{CI}, following
#' established R conventions.
#'
#' Examples:
#' \code{meanCI()}, \code{medianCI()}, \code{sdCI()}, \code{varCI()},
#' \code{quantileCI()}
#'
#' @section Statistical tests:
#' Statistical tests use lower camelCase and end with \code{Test}.
#'
#' Examples:
#' \code{shapiroFranciaTest()}, \code{andersonDarlingTest()},
#' \code{leveneTest()}, \code{jarqueBeraTest()}
#'
#' @section Plot functions:
#' Plotting functions start with the prefix \code{plot} and use lower camelCase.
#'
#' Examples:
#' \code{plotQQ()}, \code{plotECDF()}, \code{plotCorr()}, \code{plotViolin()}
#'
#' @section Classes and S3 methods:
#' Classes use \strong{UpperCamelCase}. S3 methods follow standard R conventions.
#'
#' Examples:
#' \code{Desc.numeric}, \code{PercTable}, \code{print.PercTable},
#' \code{plot.Desc.numeric}
#'
#' @details
#' Consistency and predictability take precedence over historical base R naming
#' conventions. This design choice is a key difference between DescToolsX and
#' DescTools.
#' 
#' #' Design principles
#'
#' DescToolsX follows a set of strict design principles to ensure consistency,
#' usability, and performance across the entire package.
#'
#' @section Argument order:
#' Function arguments follow a consistent and predictable order:
#' \enumerate{
#'   \item \code{x} (primary data object)
#'   \item method-specific parameters
#'   \item confidence-related parameters (e.g. \code{conf.level})
#'   \item formatting and display options
#'   \item \code{...} (additional arguments)
#' }
#'
#' This ordering is applied uniformly across statistical summary functions,
#' confidence interval functions, and plotting functions.
#'
#' @section Confidence interval functions:
#' Confidence interval functions follow the same argument order as their
#' corresponding point estimators. In particular, \code{conf.level} is always
#' used to specify the confidence level and appears explicitly as a named
#' argument.
#'
#' Examples:
#' \code{meanCI(x, conf.level = 0.95)},
#' \code{medianCI(x, conf.level = 0.95)}
#'
#' @section Plot functions:
#' Plotting functions follow the same data-first argument convention. The data
#' object is always the first argument, followed by plot-specific parameters and
#' graphical options. This ensures intuitive usage and consistent behaviour
#' across different plot types.
#'
#' Examples:
#' \code{plotQQ(x)}, \code{plotECDF(x)}, \code{plotCorr(x, method = "spearman")}
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
#' “DescToolsGraphics is listed in Depends because this package extends its
#' user-facing API and expects it to be attached. Functions used internally
#' are explicitly imported via the NAMESPACE.”
#' 
#' @keywords internal
"_PACKAGE"


