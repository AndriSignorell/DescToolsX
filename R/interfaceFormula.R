
#' Formula Interfaces - Common Arguments
#'
#' Common arguments and conventions for formula interfaces in DescToolsX.
#'
#' @name Formulas
#'
#' @param formula formula describing the design, see section
#'   **Supported forms**. Which forms a function accepts is documented on
#'   its own help page.
#'
#' @param data optional matrix or data frame (or similar; see
#'   [stats::model.frame()]) containing the variables in the formula.
#'   A matrix is converted to a data frame. If omitted or `NULL`,
#'   variables are taken from `environment(formula)`.
#'
#' @param subset optional expression specifying a subset of observations
#'   to be used in the analysis. It is evaluated in `data` first, then in
#'   `environment(formula)`, and applied before missing values are
#'   handled.
#'
#' @param na.action function specifying how missing values are handled,
#'   passed to [bedrock::resolveFormula()]. The resolver's default
#'   [stats::na.pass()] keeps incomplete cases in the model frame, so the
#'   function itself can count and report them (as [desc()] does) before
#'   removing them. [stats::na.omit()] drops them beforehand.
#'
#' @section Supported forms:
#' \describe{
#'   \item{`y ~ 1`}{one sample}
#'   \item{`Pair(x, y) ~ 1`}{two dependent (paired) samples, see
#'     [stats::Pair()]}
#'   \item{`y ~ group`}{two or more independent samples, `group` being
#'     categorical}
#'   \item{`y ~ treatment | block`}{two or more dependent samples in a
#'     block design}
#'   \item{`y ~ x`}{two numeric variables (response and predictor)}
#' }
#'
#' **`desc()`** accepts `y ~ x` for any combination of numeric and
#' categorical variables and dispatches on the types of the two columns
#' of the model frame:
#' \tabular{lll}{
#'   **y**       \tab **x**       \tab **method** \cr
#'   numeric     \tab numeric     \tab [desc.nn()] \cr
#'   numeric     \tab categorical \tab [desc.nq()] \cr
#'   categorical \tab numeric     \tab [desc.qn()] \cr
#'   categorical \tab categorical \tab [desc.qq()] \cr
#' }
#'
#' @details
#' Formula interfaces in DescToolsX are resolved consistently by
#' [bedrock::resolveFormula()]. The resolver builds the
#' [stats::model.frame()] once, applying `data`, `subset` and
#' `na.action`, and classifies the design as one-sample, two-sample
#' independent, two-sample dependent, n-sample independent, n-sample
#' dependent, or numeric-numeric. Each function states which designs it
#' accepts; any other form is rejected with an error naming the design
#' found.
#'
#' Besides the model frame, the resolver returns the row positions of the
#' retained observations in the original data. Functions that align
#' external vectors with the model frame (e.g. an ordering variable after
#' `subset` and `na.action`) use these instead of recomputing them.
#'
#' @seealso
#' [bedrock::resolveFormula()],
#' [stats::formula()],
#' [stats::model.frame()],
#' [stats::Pair()],
#' [desc()]
#'
NULL
