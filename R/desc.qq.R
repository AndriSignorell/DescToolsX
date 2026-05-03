
#' @name desc.qq
#' @aliases .desc_qq
#'
#' @title Describe Relationship: Categorical x by Categorical y
#'
#' @description
#' Computes descriptive statistics for the relationship between two
#' categorical variables \code{x} and \code{y}.
#'
#' @param x A categorical variable.
#' @param y A categorical variable.
#'
#' @details
#' This function is a wrapper around \code{\link{desc.table}} applied to
#' the contingency table \code{table(x, y)}.
#'
#' It summarizes the joint distribution of two categorical variables and
#' provides association measures and visualizations.
#'
#' \strong{Computed statistics}
#' \itemize{
#'   \item Contingency table
#'   \item Row and column percentages
#'   \item Association measures (e.g., Cramer's V, Phi)
#'   \item Optional statistical tests depending on configuration
#' }
#'
#' \strong{Implementation note}
#' Internally, \code{desc.qq(x, y)} is equivalent to:
#' \preformatted{
#' desc(table(x, y))
#' }
#'
#' @return
#' An object of class \code{"Desc.qq"} inheriting from \code{"Desc"}.
#'
#' @seealso
#' \code{\link{desc}}, \code{\link{desc.table}},
#' \code{\link{desc.qn}}, \code{\link{desc.nn}}
#'
#' @family desc
#' @concept data-description
#' @concept descriptive-statistics
#' @concept association-measures
#'
#' @rdname desc.qq
#' @usage .desc_qq(x, y)
NULL


#' @keywords internal
.desc_qq <- function(x, y) {
  desc(table(x, y))
}


#' @rdname desc.table
#' @exportS3Method
print.Desc.qq <- function(x, digits = NULL, ...) {
  
  .printHeader(x$meta)
  
  cat(x$pair$strOut)

  print.Desc.table(x$res, print_header=FALSE, ...)

}



#' @exportS3Method
#' @rdname desc.table
plot.Desc.qq <- function(x, which = 1,  ...) {
  plot.Desc.table(x, which, ...)
}

