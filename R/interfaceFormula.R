
#' Formula Interfaces - Common Arguments
#'
#' Common arguments and conventions for formula interfaces in DescToolsX.
#'
#' @name Formulas
#' 
#' @param formula formula describing the design. Depending on the function,
#'   supported forms include `y ~ 1`, `Pair(x, y) ~ 1`,
#'   `y ~ group`, `y ~ predictor`, and
#'   `y ~ treatment | block`
#'
#' @param data optional matrix or data frame (or similar; see
#'   [stats::model.frame()]) containing the variables in the
#'   formula. If omitted, variables are taken from
#'   `environment(formula)`
#'
#' @param subset optional expression specifying a subset of observations
#'   to be used in the analysis
#'
#' @param na.action function specifying how missing values are handled;
#'   passed to [bedrock::resolveFormula()]
#'
#' @details
#' Formula interfaces in DescToolsX are resolved consistently by
#' [bedrock::resolveFormula()]. The resolver constructs the
#' [stats::model.frame()] and classifies the resulting design as
#' one-sample, two-sample independent, two-sample dependent, n-sample
#' independent, n-sample dependent, or numeric-numeric.
#'
#' Individual functions may support only a subset of these designs. The
#' accepted forms are documented on the corresponding function's help page.
#' Data lookup, subsetting, and missing-value handling are delegated to
#' `resolveFormula()`.
#'
#' @seealso
#' [bedrock::resolveFormula()],
#' [stats::formula()],
#' [stats::model.frame()],
#' [stats::Pair()]
#'
NULL
