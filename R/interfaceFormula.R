
#' Formula Interface – Common Arguments
#'
#' Common formula-based interface shared by multiple functions.
#'
#' @name Formulas
#' 
#' @param formula A formula of the form \code{lhs ~ rhs}, where \code{lhs}
#'   gives the response values and \code{rhs} the corresponding groups
#'   or explanatory variables.
#'
#' @param data An optional matrix or data frame (or similar; see
#'   \code{\link[stats]{model.frame}}) containing the variables in the
#'   formula. By default the variables are taken from
#'   \code{environment(formula)}.
#'
#' @param subset An optional vector specifying a subset of observations
#'   to be used in the analysis.
#'
#' @param na.action A function which indicates what should happen when
#'   the data contain \code{NA}s. Defaults to
#'   \code{getOption("na.action")}.
#'
#' @details
#' Formula interfaces are evaluated using \code{\link[stats]{model.frame}},
#' following standard R conventions.
#' The left-hand side of the formula must contain the response variable.
#' The right-hand side typically specifies a grouping or explanatory variable.
#' Only formulas with a single response and at least one explanatory
#' variable are supported.
#'
#' See also:
#' \itemize{
#'   \item \code{\link[stats]{formula}}
#'   \item \code{\link[stats]{model.frame}}
#'   \item \code{\link[stats]{terms}}
#' }
#'
#' @keywords internal
NULL
