#' Association Measures – Common Interface
#'
#' All association measures in this package share a common interface. 
#'
#' @name Association
#' @details
#' The association is defined between two variables that can be 
#' provided to the calculation functions in different ways. 
#' If only \code{x} is passed, this can either be a matrix, which 
#' is then interpreted as a contingency table (this seems in the case of frequency
#' data the natural interpretation and is by the way also what
#' \code{\link{chisq.test}} expects). However, it can also be a data.frame or  
#' a list, which must then contain exactly 2 elements containing the data. 
#' Alternatively, two single data vectors \code{x} and \code{y} can be passed. 
#' The two element arguments are typically processed by forming a contingency 
#' table with \code{\link{table}(x, y, ...)}. \code{NAs} are by default handled the same 
#' way as the function does, so \code{NAs} omitted. 
#' 
#' If the measure should be calculated pairwise for a set of variables 
#' \code{\link{pairApply}} can be used. This easily allows to create matrices 
#' of association measures (the same way as the \code{cor} does). \code{NAs} 
#' again are by default omitted pairwise, which corresponds to the 
#' \code{pairwise.complete} option of \code{\link{cor}}. 
#' Use \code{\link{complete.cases}}, if only the complete
#' cases of a \code{data.frame} are to be used. (see examples)
#' 
#' Most functions support calculation of confidence intervals.
#' These can be requested by setting \code{conf.level} to the desired value (usually 0.95). 
#' If it is set to \code{NA}, no confidence interval is computed. 
#' One-sided confidence intervals 
#' can be controlled using the sides argument. The definition is following the
#' handling in statistical tests. Alternative hypothesis: \code{"two.sided"}, 
#' \code{"left"}, or \code{"right"}. Frequently there is a classic and 
#' a bootsrap approach (\code{"classic"}, \code{"boot"}). 
#' However most measures have their own specific confidence intervals methods.
#' 
#' Some association measures define additional parameters such as \code{direction},
#' \code{base}, or \code{correct}. Those are documented with the respective
#' functions.
#' 
#' 
#' \strong{Function List}
#' 
#' Following association measures are implemented in \strong{DescToolsX}:
#'  \tabular{ll}{
#'    \verb{  }\link{cramerV}             \tab Cramer's V \cr
#'    \verb{  }\link{lambda}\verb{  } \tab Goodman's Lambda \cr
#'    }
#'
#' @param x Either a vector of observations, a two-column object
#'   (matrix, data.frame or list), or a contingency table.
#' @param y Optional second vector. If \code{x} is a contingency table,
#'   \code{y} must be \code{NULL}.
#' @param conf.level Confidence level for confidence intervals.
#'   If \code{NA}, no confidence interval is computed.
#' @param sides Alternative hypothesis:
#'   \code{"two.sided"}, \code{"left"}, or \code{"right"}.
#' @param method Method used for inference. Available options depend
#'   on the selected association measure.
#' @param ... Additional arguments passed to internal helper functions,
#'   such as bootstrap routines (\code{\link[boot]{boot}}) or 
#'   table handling (\code{\link[base]{table}}) (e.g. \code{useNA}, \code{R}).
#'
#' @references 
#' Cramer, H. (1946) \emph{Mathematical Methods of Statistics}. Princeton
#' University Press
#' 
#' Agresti, Alan (1996) \emph{Introduction to categorical data analysis}. NY:
#' John Wiley and Sons
#' 
#' @keywords internal
NULL
