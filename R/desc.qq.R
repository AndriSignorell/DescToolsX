
#' @name desc.qq
#' @aliases .descQQ
#'
#' @title Describe Relationship: Categorical x by Categorical y
#'
#' @description
#' Computes, prints and plots descriptive statistics for the relationship
#' between two categorical variables `x` and `y`. The function is
#' dispatched automatically by `desc(y ~ x, data)` when both variables
#' are categorical.
#'
#' @param x a categorical variable for `.descQQ()`, or an object of class
#'   `"Desc.qq"` for the print and plot methods
#' @param y a categorical variable
#' @param digits currently unused
#' @param which integer vector selecting the plots to draw, passed on to
#'   [pharos::plot.Desc.table()], see section **Plots**. Default `1`.
#' @param \dots further arguments. In `.descQQ()` passed to [desc()], in
#'   `print()` to [print.Desc.table()], in `plot()` to
#'   [pharos::plot.Desc.table()].
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
#' @section Plots:
#' `plot()` labels the table dimensions with the variable names and hands
#' over to [pharos::plot.Desc.table()], the plot method for contingency
#' tables. Its help page documents the displays selected by `which` and
#' all further arguments; the displays themselves are drawn by
#' [pharos::plotMosaic()], [pharos::plotAssoc()] and
#' [pharos::plotHeatmap()], among others.
#'
#' `main` defaults to the title stored in the object.
#'
#' @return `.descQQ()` returns an object of class `c("Desc.qq", "Desc")`.
#' The plot method returns the value of [pharos::plot.Desc.table()].
#'
#' @seealso
#' [desc()], [desc.table()],
#' [desc.qn()], [desc.nq()], [desc.nn()]
#'
#' Plot method: [pharos::plot.Desc.table()]
#'
#' @family desc
#' @concept data-description
#' @concept descriptive-statistics
#' @concept association-measures
#' @concept contingency table mosaic heatmap
#'
#' @examples
#' # basic usage via desc()
#' desc(quality ~ area, Pizza)
#'
#' # store result, print and plot separately
#' d <- desc(quality ~ area, Pizza, plotit = FALSE)
#' d
#'
#' # the plots, see pharos::plot.Desc.table()
#' plot(d, which = 1)
#' plot(d, which = 2)
#' plot(d, which = 3)
#' plot(d, which = 4)                     # association plot
#' plot(d, which = 5)                     # heatmap
#'
#' # pipe
#' desc(quality ~ area, Pizza) |> plot(which = 4)
#'
#' @rdname desc.qq
#' @usage .descQQ(x, y, ...)
NULL


.descQQ <- function(x, y, ...) {
  desc(table(x, y), ...)
}


#' @rdname desc.qq
#' @exportS3Method
print.Desc.qq <- function(x, digits = NULL, ...) {
  
  .printHeader(x$meta)
  
  cat(x$pair$strOut)

  # the inner table carries its own plotit (from the option default);
  # only the pair is plotted, once
  res <- x$res
  res$meta$plotit <- FALSE
  print.Desc.table(res, printHeader=FALSE, ...)

  .plotIfRequested(x)
}


#' @param main main title for the plot; defaults to the title stored in
#' `x$meta$main`
#' @exportS3Method
#' @rdname desc.qq
plot.Desc.qq <- function(x, main = x$meta$main, which = 1, ...) {
  
  names(dimnames(x$res$tab)) <- c(
    x$meta$xname,
    x$meta$yname
  )
  
  plot.Desc.table(x$res, main = main, which = which, ...)
  
}
