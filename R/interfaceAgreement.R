#' Agreement Measures - Common Interface
#'
#' All agreement measures in this package share a common interface. 
#'
#' @name Agreement
#' @details
#' Agreement is defined between two or more raters evaluating the same
#' subjects. The data can be provided in different formats.
#'
#' If only \code{x} is passed, it must be a matrix or data.frame where:
#' \itemize{
#'   \item rows represent subjects
#'   \item columns represent raters
#' }
#'
#' Alternatively, for two raters only, two vectors \code{x} and \code{y}
#' of equal length can be supplied.
#'
#' Missing values (\code{NA}) are by default omitted pairwise,
#' i.e. only subjects rated by all involved raters are used.
#' If complete-case analysis across all raters is required,
#' use \code{\link{complete.cases}} beforehand.
#'
#' Agreement measures depend on:
#' \itemize{
#'   \item Number of raters (2 vs. >2)
#'   \item Scale type (nominal, ordinal, metric)
#' }
#'
#' For ordinal data, weighted approaches are available.
#'
#' Most functions support confidence intervals via \code{conf.level}.
#' If \code{conf.level = NA}, no interval is computed.
#' One-sided intervals can be requested via \code{sides}
#' ("two.sided", "left", "right").
#'
#' Inference methods may include classical large-sample approaches
#' or bootstrap methods ("boot"). Specific measures may provide
#' additional estimation procedures.
#'
#' Some agreement measures define additional parameters such as:
#' \itemize{
#'   \item \code{weights} (for ordinal agreement)
#'   \item \code{model} (for ICC variants)
#'   \item \code{type} (consistency vs. absolute agreement)
#'   \item \code{unit} (single vs. average rating)
#' }
#'
#'
#' \strong{Function List}
#'
#' Following agreement measures are implemented in \strong{DescToolsX}:
#'
#' \tabular{ll}{
#'   \verb{  }\link{cohenKappa}       \tab Cohen's Kappa \cr
#'   \verb{  }\code{scottsPi}         \tab Scott's Pi \cr
#'   \verb{  }\code{gwetAC1}          \tab Gwet's AC1 / AC2 \cr
#'   \verb{  }\link{kappaM}      \tab Fleiss' Kappa \cr
#'   \verb{  }\link{kappaM}      \tab Light's Kappa \cr
#'   \verb{  }\link{krippAlpha}\tab Krippendorff's Alpha \cr
#'   \verb{  }\link{kendallW}         \tab Kendall's W \cr
#'   \verb{  }\link{icc}              \tab Intraclass Correlation (ICC) \cr
#'   \verb{  }\link{ccc}           \tab Lin's Concordance Correlation \cr
#' }
#'
#' @param x A matrix or data.frame of ratings (subjects x raters),
#'   or a single vector if \code{y} is provided.
#' @param y Optional second vector (only for two raters).
#' @param conf.level Confidence level for confidence intervals.
#'   If \code{NA}, no confidence interval is computed.
#' @param sides Alternative hypothesis:
#'   \code{"two.sided"}, \code{"left"}, or \code{"right"}.
#' @param method Method used for inference (depends on measure).
#' @param weights Optional weighting scheme for ordinal data
#'   (e.g. "linear", "quadratic").
#' @param model ICC model ("oneway", "twoway").
#' @param type ICC type ("consistency", "agreement").
#' @param unit ICC unit ("single", "average").
#' @param ... Additional arguments passed to helper functions
#'   (e.g. bootstrap routines).
#'
#' @references
#' Cohen, J. (1960) A coefficient of agreement for nominal scales.
#' Educational and Psychological Measurement.
#'
#' Fleiss, J. L. (1971) Measuring nominal scale agreement among many raters.
#' Psychological Bulletin.
#'
#' Krippendorff, K. (2004) Content Analysis.
#'
#' Shrout, P. E., & Fleiss, J. L. (1979) Intraclass correlations.
#'
#' Lin, L. I.-K. (1989) A concordance correlation coefficient.
#'
#' @keywords internal
NULL
