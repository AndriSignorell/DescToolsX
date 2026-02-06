

#' Lagged Quotients
#' 
#' Returns suitably lagged and iterated quotients
#' 
#' \code{\link{NA}}'s propagate.
#' 
#' @param x a numeric vector or matrix containing the values to be used for
#' calculating the quotients.
#' @param lag an integer indicating which lag to use.
#' @param quotients an integer indicating the order of the quotient.
#' @param \dots further arguments to be passed to or from methods.
#' @return If \code{x} is a vector of length \code{n} and \code{quotients = 1},
#' then the computed result is equal to the successive quotients
#' \code{x[(1+lag):n] - x[1:(n-lag)]}.
#' 
#' If \code{quotients} is larger than one this algorithm is applied recursively
#' to \code{x}.  Note that the returned value is a vector which is shorter than
#' \code{x}.
#' 
#' If \code{x} is a matrix then the division operations are carried out on each
#' column separately.
#' @seealso \code{\link{diff}}
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The
#' New S Language}.  Wadsworth & Brooks/Cole.
#' @keywords arith
#' @examples
#' 
#' quot(1:10, 2)
#' quot(1:10, 2, 2)
#' x <- cumprod(cumprod(1:10))
#' quot(x, lag = 2)
#' quot(x, quotients = 2)
#' 


#' @export
quot <- function (x, lag = 1L, quotients = 1L, ...) {
  
  ismat <- is.matrix(x)
  xlen <- if (ismat) 
    dim(x)[1L]
  else length(x)
  if (length(lag) != 1L || length(quotients) > 1L || lag < 
      1L || quotients < 1L) 
    stop("'lag' and 'quotients' must be integers >= 1")
  if (lag * quotients >= xlen) 
    return(x[0L])
  r <- unclass(x)
  i1 <- -seq_len(lag)
  if (ismat) 
    for (i in seq_len(quotients)) 
      r <- r[i1, , drop = FALSE] / r[-nrow(r):-(nrow(r) - lag + 1L), , drop = FALSE]
  else 
    for (i in seq_len(quotients)) 
      r <- r[i1] / r[-length(r):-(length(r) - lag + 1L)]
  
  class(r) <- oldClass(x)
  r
  
}
