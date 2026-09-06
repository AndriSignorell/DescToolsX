
#' @name desc.qq
#' @aliases .descQQ
#'
#' @title Describe Relationship: Categorical x by Categorical y
#'
#' @description
#' Computes descriptive statistics for the relationship between two
#' categorical variables `x` and `y`.
#'
#' @param x a categorical variable
#' @param y a categorical variable
#' @param ... further arguments, currently unused
#'
#' @details
#' This function is a wrapper around [desc.table()] applied to
#' the contingency table `table(x, y)`.
#'
#' It summarizes the joint distribution of two categorical variables and
#' provides association measures and visualizations.
#'
#' **Computed statistics**
#' \itemize{
#'   \item Contingency table
#'   \item Row and column percentages
#'   \item Association measures (e.g., Cramer's V, Phi)
#'   \item Optional statistical tests depending on configuration
#' }
#'
#' **Implementation note**
#' Internally, `desc.qq(x, y)` is equivalent to:
#' \preformatted{
#' desc(table(x, y))
#' }
#'
#' @return an object of class `c("Desc.qq", "Desc")`
#'
#' @seealso
#' [desc], [desc.table],
#' [desc.qn], [desc.nn], [pharos::plot.Desc.table]
#'
#' @family desc
#' @concept data-description
#' @concept descriptive-statistics
#' @concept association-measures
#'
#' @rdname desc.qq
#' @usage .descQQ(x, y, ...)
NULL


.descQQ <- function(x, y, ...) {
  desc(table(x, y), ...)
}


#' @rdname desc.table
#' @exportS3Method
print.Desc.qq <- function(x, digits = NULL, ...) {
  
  .printHeader(x$meta)
  
  cat(x$pair$strOut)

  print.Desc.table(x$res, print_header=FALSE, ...)

}


#' @param main main title for the plot; defaults to the title stored in
#' `x$meta$main`
#' @exportS3Method
#' @rdname desc.table
plot.Desc.qq <- function(x, main = x$meta$main, which = 1, ...) {
  
  names(dimnames(x$res$tab)) <- c(
    x$meta$xname,
    x$meta$yname
  )
  
  plot.Desc.table(x$res, main = main, which = which, ...)
  
}
