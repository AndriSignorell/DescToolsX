
#' Returns the Left Or the Right Part Of a String 
#' 
#' Returns the left part or the right part of a string. The number of
#' characters are defined by the argument \code{n}. If \code{n} is negative,
#' this number of characters will be cut off from the other side. 
#' 
#' The functions \code{strLeft} and \code{strRight} are simple wrappers to
#' \code{substr}.
#' 
#' @name strLeftRight
#' @aliases strRight strLeft
#' @param x a vector of strings. 
#' @param n a positive or a negative integer, the number of characters to cut.
#' If n is negative, this number of characters will be cut off from the right
#' with \code{strLeft} and from the right with \code{strRight}. \cr n will be
#' recycled. 
#' @return the left (right) n characters of x 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{substr}}, \code{\link{strTrim}} 
#' @keywords manip character
#' @examples
#' 
#' strLeft("Hello world!", n=5)
#' strLeft("Hello world!", n=-5)
#' 
#' strRight("Hello world!", n=6)
#' strRight("Hello world!", n=-6)
#' 
#' strLeft(c("Lorem", "ipsum", "dolor","sit","amet"), n=2)
#' 
#' strRight(c("Lorem", "ipsum", "dolor","sit","amet"), n=c(2,3))
#' 


#' @rdname strLeftRight
#' @export
strRight <- function(x, n) {
  n <- rep(n, length.out=length(x))
  sapply(seq_along(x), function(i) {
    if(n[i] >= 0)
      substr(x[i], (nchar(x[i]) - n[i]+1L), nchar(x[i]))
    else
      substr(x[i], - n[i]+1L, nchar(x[i]))
  }  )
}


#' @rdname strLeftRight
#' @export
strLeft <- function(x, n) {
  n <- rep(n, length.out=length(x))
  sapply(seq_along(x), function(i) {
    if(n[i] >= 0)
      substr(x[i], 0, n[i])
    else
      substr(x[i], 0, nchar(x[i]) + n[i])
  } )
}

