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
#' One-sided intervals can be requested via \code{sides}, which names the
#' side carrying the finite bound; see \link{ConfidenceIntervals}.
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
#'   \verb{  }\link{cronbachAlpha}    \tab Cronbach's Alpha \cr
#'   \verb{  }\link{kappaM}          \tab Fleiss' and Light's Kappa \cr
#'   \verb{  }\link{krippAlpha}\tab Krippendorff's Alpha \cr
#'   \verb{  }\link{kendallW}         \tab Kendall's W \cr
#'   \verb{  }\link{icc}              \tab Intraclass Correlation (ICC) \cr
#'   \verb{  }\link{ccc}           \tab Lin's Concordance Correlation \cr
#' }
#'
#' @param x matrix or data frame of ratings (subjects x raters),
#'   or a single vector if \code{y} is provided
#' @param y optional second vector for two raters
#' @param conf.level confidence level for confidence intervals.
#'   If \code{NA}, no confidence interval is computed.
#' @param sides character string specifying the side of the confidence
#'   interval, one of \code{"two.sided"} (default), \code{"left"} or
#'   \code{"right"}. It names the side on which the \emph{finite} bound
#'   lies, not the direction of an alternative hypothesis - see
#'   \link{ConfidenceIntervals} for the full definition and for the
#'   difference to \pkg{DescTools}.
#' @param method method used for inference; available methods depend on the
#' measure
#' @param weights optional weighting scheme for ordinal data
#'   (e.g. "linear", "quadratic")
#' @param model model for the ICC, \code{"oneway"} or \code{"twoway"}
#' @param type type of ICC, \code{"consistency"} or \code{"agreement"}
#' @param unit unit of the ICC, \code{"single"} or \code{"average"}
#' @param ... additional arguments passed to helper functions
#'   (e.g. bootstrap routines)
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
#' Shrout, P. E. and Fleiss, J. L. (1979) Intraclass correlations.
#'
#' Lin, L. I.-K. (1989) A concordance correlation coefficient.
#'
NULL
