
#' The (weighted) Interquartile Range 
#' 
#' computes interquartile range of the x values. Weights are supported. 
#' 
#' This implementation is based on [quantileX()] function, which
#' allows to define weights. 
#' 
#' @param x numeric vector
#' @param weights optional numeric vector giving the sample weights
#' @param na.rm logical; whether to remove missing values
#' @param type an integer selecting one of the many quantile algorithms, see
#' [quantileX()]
#'
#' @return numeric scalar containing the interquartile range
#' 
#' 
#' @examples
#' 
#' x <- c(3.7,3.3,3.5,2.8)
#' w <- c(5, 5, 4, 1)/15
#' 
#' iqrX(x=x, weights=w)
#' 
#' @seealso [medianX], [quantileX], [IQR], [quantile] 
#' 
#' @family dispersion
#' @concept dispersion
#' @export
iqrX <- function (x, weights = NULL, na.rm = FALSE, type = NULL) {

  # type = NULL means "the right default for the branch taken":
  #
  #   unweighted -> 7, matching IQR() and quantile()
  #   weighted   -> 5, which reads the weights as RELATIVE and therefore
  #                  depends only on their ratios
  #
  # The former default of 7 was passed to quantileX() unchanged, where it
  # reads the weights as replication counts. Weights normalized to sum to
  # 1 - as in this function's own example, w <- c(5, 5, 4, 1)/15 - then
  # make every quantile collapse onto max(x) and the IQR come out as 0.
  # medianX() was given the same treatment for the same reason.
  #
  # Passing type explicitly still selects the algorithm; the default just
  # stops depending on how the caller happened to scale the weights.
  if(is.null(weights))
    IQR(x = x, na.rm = na.rm, type = if(is.null(type)) 7 else type)

  else
    # unname(): diff() carries the name of the second element through, so
    # the weighted branch returned a scalar labelled "75%" while the
    # unweighted one returned a bare number. Same function, two shapes.
    unname(diff(quantileX(x, weights = weights, probs = c(0.25, 0.75),
                          na.rm = na.rm,
                          type = if(is.null(type)) 5 else type)))

}
